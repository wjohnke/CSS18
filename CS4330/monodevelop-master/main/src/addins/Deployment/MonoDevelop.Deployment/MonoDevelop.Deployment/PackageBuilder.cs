//
// PackageBuilder.cs
//
// Author:
//   Lluis Sanchez Gual
//
// Copyright (C) 2006 Novell, Inc (http://www.novell.com)
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//


using System;
using System.Collections.Generic;
using MonoDevelop.Core.Serialization;
using MonoDevelop.Core;
using MonoDevelop.Core.ProgressMonitoring;
using MonoDevelop.Projects;
using MonoDevelop.Ide;
using System.Threading.Tasks;
using System.Linq;

namespace MonoDevelop.Deployment
{
	[DataItem (FallbackType=typeof(UnknownPackageBuilder))]
	public class PackageBuilder: IDirectoryResolver
	{
		[ItemProperty ("ChildEntries")]
		List<SolutionItemReference> childEntries = new List<SolutionItemReference> ();
		
		[ItemProperty ("ExcludedFiles")]
		[ItemProperty ("File", Scope="*")]
		List<string> excludedFiles;
		
		[ItemProperty ("RootEntry")]
		SolutionItemReference rootEntry;
		
		List<SolutionFolderItem> childCombineEntries;
		SolutionFolderItem rootSolutionItem;
		
		public PackageBuilder ()
		{
		}
		
		public virtual string Description {
			get { return GettextCatalog.GetString ("Package"); } 
		}
		
		public virtual string Icon {
			get { return "md-package"; }
		}
		
		public virtual string DefaultName {
			get {
				return Description;
			}
		}
		
		public virtual string Validate ()
		{
			return null;
		}
		
		internal bool Build (ProgressMonitor monitor)
		{
			monitor.BeginTask ("Package: " + Description, 1);
			DeployContext ctx = null;
			try {
				ctx = CreateDeployContext ();
				if (ctx != null)
					ctx.FileFilter = this;
				if (!OnBuild (monitor, ctx)) {
					return false;
				}
			} catch (Exception ex) {
				monitor.ReportError ("Package creation failed", ex);
				LoggingService.LogError ("Package creation failed", ex);
				return false;
			} finally {
				monitor.EndTask ();
				if (ctx != null)
					ctx.Dispose ();
			}
			return true;
		}
		
		public virtual bool CanBuild (SolutionFolderItem entry)
		{
			return true;
		}
		
		public virtual void InitializeSettings (SolutionFolderItem entry)
		{
		}
		
		public PackageBuilder Clone ()
		{
			PackageBuilder d = (PackageBuilder) Activator.CreateInstance (GetType());
			d.CopyFrom (this);
			return d;
		}
		
		public virtual void CopyFrom (PackageBuilder other)
		{
			childEntries = new List<SolutionItemReference> (other.childEntries);
			rootEntry = other.rootEntry;

			if (other.childCombineEntries != null)
				childCombineEntries = new List<SolutionFolderItem> (other.childCombineEntries);
			else
				childCombineEntries = null;
			if (other.excludedFiles != null)
				excludedFiles = new List<string> (other.excludedFiles);
			else
				excludedFiles = null;
			rootSolutionItem = other.rootSolutionItem;
		}
		
		public virtual PackageBuilder[] CreateDefaultBuilders ()
		{
			return new PackageBuilder [0];
		}
		
		protected virtual bool OnBuild (ProgressMonitor monitor, DeployContext ctx)
		{
			return true;
		}
		
		string IDirectoryResolver.GetDirectory (DeployContext ctx, string folderId)
		{
			return OnResolveDirectory (ctx, folderId);
		}
		
		protected virtual string OnResolveDirectory (DeployContext ctx, string folderId)
		{
			return null;
		}
		
		public virtual DeployContext CreateDeployContext ()
		{
			return new DeployContext (this, DeployService.CurrentPlatform, null);
		}
		
		public void SetSolutionItem (SolutionFolderItem entry)
		{
			SetSolutionItem (entry, null);
		}
		
		public void SetSolutionItem (SolutionFolderItem rootSolutionItem, IEnumerable<SolutionFolderItem> childEntries)
		{
			this.rootSolutionItem = rootSolutionItem;
			childCombineEntries = new List<SolutionFolderItem> ();
			
			if (childEntries != null)
			    childCombineEntries.AddRange (childEntries);
			
			UpdateEntryNames ();
			InitializeSettings (rootSolutionItem);
		}
		
		internal void SetSolutionItem (SolutionItemReference siRoot, SolutionItemReference[] children)
		{
			rootEntry = siRoot;
			childEntries.Clear ();
			foreach (SolutionItemReference e in children)
				childEntries.Add (e);
		}
		
		void UpdateEntryNames ()
		{
			this.rootEntry = new SolutionItemReference (rootSolutionItem);
			this.childEntries.Clear ();
			foreach (SolutionFolderItem e in childCombineEntries)
				childEntries.Add (new SolutionItemReference (e));
		}
		
		public SolutionFolderItem RootSolutionItem {
			get {
				if (rootSolutionItem == null && rootEntry != null)
					rootSolutionItem = GetEntry (rootEntry);
				return rootSolutionItem; 
			}
		}
		
		public Solution Solution {
			get {
				return RootSolutionItem != null ? RootSolutionItem.ParentSolution : null;
			}
		}
		
		public void AddEntry (SolutionFolderItem entry)
		{
			SolutionItemReference fp = new SolutionItemReference (entry);
			foreach (SolutionItemReference s in childEntries)
				if (s.Equals (fp))
					return;
			
			if (rootEntry == fp)
				return;
			
			List<SolutionFolderItem> list = new List<SolutionFolderItem> ();
			if (RootSolutionItem != null)
				list.Add (RootSolutionItem);
			list.AddRange (GetChildEntries());
			list.Add (entry);
			
			rootSolutionItem = GetCommonSolutionItem (list);
			list.Remove (rootSolutionItem);
			
			foreach (SolutionFolderItem e in list.ToArray ()) {
				SolutionFolderItem ce = e.ParentFolder;
				while (ce != rootSolutionItem) {
					if (!list.Contains (ce))
						list.Add (ce);
					ce = ce.ParentFolder;
				}
			}
			childCombineEntries = list;
			UpdateEntryNames ();
		}
		
		public SolutionFolderItem[] GetChildEntries ()
		{
			if (childCombineEntries != null)
				return childCombineEntries.ToArray ();
			
			childCombineEntries = new List<SolutionFolderItem> ();
			
			foreach (SolutionItemReference en in childEntries) {
				SolutionFolderItem re = GetEntry (en);
				if (re != null && !(re is UnknownSolutionItem))
					childCombineEntries.Add (re);
			}
			return childCombineEntries.ToArray ();
		}
		
		public SolutionFolderItem[] GetAllEntries ()
		{
			List<SolutionFolderItem> list = new List<SolutionFolderItem> ();
			if (RootSolutionItem != null)
				list.Add (RootSolutionItem);
			list.AddRange (GetChildEntries ());
			return list.ToArray ();
		}
		
		SolutionFolderItem GetEntry (SolutionItemReference reference)
		{
			if (IdeApp.IsInitialized)
				return Services.ProjectService.ReadSolutionItem (new ProgressMonitor (), reference, IdeApp.Workspace.Items.ToArray ()).Result;
			else
				return Services.ProjectService.ReadSolutionItem (new ProgressMonitor (), reference).Result;
		}
		
		public virtual DeployFileCollection GetDeployFiles (DeployContext ctx, ConfigurationSelector configuration)
		{
			return DeployService.GetDeployFiles (ctx, GetAllEntries (), configuration);
		}
		
		public virtual string[] GetSupportedConfigurations ()
		{
			if (Solution != null) {
				ICollection<string> col = Solution.GetConfigurations ();
				string[] arr = new string [col.Count];
				col.CopyTo (arr, 0);
				return arr;
			}
			else
				return new string [0];
		}
		
		public bool IsFileIncluded (DeployFile file)
		{
			if (excludedFiles == null)
				return true;
			return !excludedFiles.Contains (GetKey (file));
		}
		
		public void SetFileIncluded (DeployFile file, bool included)
		{
			if (excludedFiles == null)
				excludedFiles = new List<string> ();
			excludedFiles.Remove (GetKey (file));
			if (!included)
				excludedFiles.Add (GetKey (file));
		}
		
		string GetKey (DeployFile file)
		{
			return file.SourceSolutionItem.Name + "," + file.TargetDirectoryID + "," + file.RelativeTargetPath;
		}
		
		
		internal static SolutionFolderItem GetCommonSolutionItem (IEnumerable<SolutionFolderItem> entries)
		{
			SolutionFolderItem common = null;
			foreach (SolutionFolderItem it in entries) {
				if (common == null)
					common = it;
				else
					return it.ParentSolution.RootFolder;
			}
			return common;
		}
	}
}
