// 
// CodeGenerator.cs
//  
// Author:
//       Mike Krüger <mkrueger@novell.com>
// 
// Copyright (c) 2010 Novell, Inc (http://www.novell.com)
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Mono.Addins;
using ICSharpCode.NRefactory.TypeSystem;
using MonoDevelop.Core.AddIns;
using MonoDevelop.Ide.TypeSystem;
using ICSharpCode.NRefactory;
using MonoDevelop.Projects.Policies;
using MonoDevelop.Ide.Extensions;
using MonoDevelop.Ide.Editor;

namespace MonoDevelop.Ide.TypeSystem
{
	public abstract class CodeGenerator
	{
		static Dictionary<string, MimeTypeExtensionNode> generators = new Dictionary<string, MimeTypeExtensionNode> ();
		
		public bool AutoIndent {
			get;
			set;
		}
		
		public bool UseSpaceIndent {
			get;
			set;
		}

		public string EolMarker {
			get;
			set;
		}

		public int TabSize {
			get;
			set;
		}

		public virtual PolicyContainer PolicyParent {
			get;
			set;
		}

		public static CodeGenerator CreateGenerator (TextEditor editor, DocumentContext documentContext)
		{
			MimeTypeExtensionNode node;
			if (!generators.TryGetValue (editor.MimeType, out node))
				return null;

			var result = (CodeGenerator)node.CreateInstance ();

			result.UseSpaceIndent = editor.Options.TabsToSpaces;
			result.EolMarker = editor.EolMarker;
			result.TabSize = editor.Options.TabSize;

			return result;
		}

		public static CodeGenerator CreateGenerator (Ide.Gui.Document doc)
		{
			return CreateGenerator (doc.Editor, doc);
		}

		public static CodeGenerator CreateGenerator (ITextDocument editor, ICompilation compilation)
		{
			MimeTypeExtensionNode node;
			if (!generators.TryGetValue (editor.MimeType, out node))
				return null;

			var result = (CodeGenerator)node.CreateInstance ();

			//result.UseSpaceIndent = editor.Options.TabsToSpaces;
			result.EolMarker = editor.GetEolMarker ();
			//result.TabSize = editor.Options.TabSize;
			//result.Compilation = compilation;

			return result;
		}

		protected void AppendLine (StringBuilder sb)
		{
			sb.Append (EolMarker);
		}

		protected string GetIndent (int indentLevel)
		{
			if (UseSpaceIndent) 
				return new string (' ', indentLevel * TabSize);

			return new string ('\t', indentLevel);
		}

		public static bool HasGenerator (string mimeType)
		{
			return generators.ContainsKey (mimeType);
		}

		static CodeGenerator ()
		{
			AddinManager.AddExtensionNodeHandler ("/MonoDevelop/TypeSystem/CodeGenerators", delegate (object sender, ExtensionNodeEventArgs args) {
				var node = (MimeTypeExtensionNode)args.ExtensionNode;
				switch (args.Change) {
				case ExtensionChange.Add:
					AddGenerator (node);
					break;
				case ExtensionChange.Remove:
					RemoveGenerator (node);
					break;
				}
			});
		}

		public int IndentLevel {
			get;
			set;
		}

		public CodeGenerator ()
		{
			IndentLevel = -1;
			AutoIndent = true;
		}

		public static void AddGenerator (MimeTypeExtensionNode node)
		{
			generators [node.MimeType] = node;
		}

		public static void RemoveGenerator (MimeTypeExtensionNode node)
		{
			generators.Remove (node.MimeType);
		}

		static bool CompareMethods (IMethod interfaceMethod, IMethod typeMethod)
		{
			if (typeMethod.IsExplicitInterfaceImplementation)
				return typeMethod.ImplementedInterfaceMembers.Any (m => m.Equals (interfaceMethod));
			return SignatureComparer.Ordinal.Equals (interfaceMethod, typeMethod);
		}

		public abstract string WrapInRegions (string regionName, string text);

		public abstract void AddGlobalNamespaceImport (TextEditor editor, DocumentContext context, string nsName);
		public abstract void AddLocalNamespaceImport (TextEditor editor, DocumentContext context, string nsName, TextLocation caretLocation);

		public void AddGlobalNamespaceImport (MonoDevelop.Ide.Gui.Document doc, string nsName)
		{
			if (doc == null)
				throw new ArgumentNullException ("doc");
			AddGlobalNamespaceImport (doc.Editor, doc, nsName);
		}

		public void AddLocalNamespaceImport (MonoDevelop.Ide.Gui.Document doc, string nsName, TextLocation caretLocation)
		{
			if (doc == null)
				throw new ArgumentNullException ("doc");
			AddLocalNamespaceImport (doc.Editor, doc, nsName, caretLocation);
		}


		//public abstract string GetShortTypeString (TextEditor editor, DocumentContext context, IType type);

		public abstract void CompleteStatement (MonoDevelop.Ide.Gui.Document doc);
	}

	public class CodeGeneratorMemberResult
	{
		public CodeGeneratorMemberResult (string code) : this (code, null)
		{
		}

		public CodeGeneratorMemberResult (string code, int bodyStartOffset, int bodyEndOffset)
		{
			this.Code = code;
			this.BodyRegions = new CodeGeneratorBodyRegion[] {
				new CodeGeneratorBodyRegion (bodyStartOffset, bodyEndOffset)
			};
		}

		public CodeGeneratorMemberResult (string code, IList<CodeGeneratorBodyRegion> bodyRegions)
		{
			this.Code = code;
			this.BodyRegions = bodyRegions ?? new CodeGeneratorBodyRegion[0];
		}

		public string Code { get; private set; }

		public IList<CodeGeneratorBodyRegion> BodyRegions { get; private set; }
	}

	public class CodeGeneratorBodyRegion
	{
		public CodeGeneratorBodyRegion (int startOffset, int endOffset)
		{
			this.StartOffset = startOffset;
			this.EndOffset = endOffset;
		}

		public int StartOffset { get; private set; }

		public int EndOffset { get; private set; }

		public int Length {
			get {
				return EndOffset - StartOffset;
			}
		}

		public bool IsValid {
			get {
				return StartOffset >= 0 && Length >= 0;
			}
		}
	}
}
