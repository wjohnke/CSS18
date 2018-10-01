// SolutionFolderItemCollection.cs
//
// Author:
//   Lluis Sanchez Gual <lluis@novell.com>
//
// Copyright (c) 2008 Novell, Inc (http://www.novell.com)
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
//
//

using System;
using System.Collections.ObjectModel;
using System.Collections.Generic;

namespace MonoDevelop.Projects
{
	public class SolutionFolderItemCollection: ItemCollection<SolutionFolderItem>
	{
		SolutionFolder parentFolder;
		
		public SolutionFolderItemCollection ()
		{
		}
		
		internal SolutionFolderItemCollection (SolutionFolder parentFolder)
		{
			this.parentFolder = parentFolder;
		}
		
		internal void Replace (SolutionFolderItem item, SolutionFolderItem newItem)
		{
			int i = IndexOf (item);
			List = List.SetItem (i, newItem);
			newItem.ParentFolder = parentFolder;
		}
		
		protected override void OnItemsAdded (IEnumerable<SolutionFolderItem> items)
		{
			if (parentFolder != null) {
				foreach (var item in items) {
					// If the item belongs to another solution, remove it from there.
					// If it belongs to the same solution, just move the item, avoiding
					// the global item added/removed events.
					if (item.ParentFolder != null && item.ParentSolution != null) {
						if (item.ParentSolution != parentFolder.ParentSolution)
							item.ParentFolder.Items.Remove (item);
						else {
							SolutionFolder oldFolder = item.ParentFolder;
							item.ParentFolder = null;
							oldFolder.Items.InternalRemove (item);
							oldFolder.NotifyItemRemoved (item, false);
							item.ParentFolder = parentFolder;
							parentFolder.NotifyItemAdded (item, false);
							return;
						}
					}
					item.ParentFolder = parentFolder;
					parentFolder.NotifyItemAdded (item, true);
				}
			}
		}
		
		protected override void OnItemsRemoved (IEnumerable<SolutionFolderItem> items)
		{
			if (parentFolder != null) {
				foreach (var item in items) {
					item.ParentFolder = null;
					parentFolder.NotifyItemRemoved (item, true);
				}
			}
		}

		internal void InternalRemove (SolutionFolderItem item)
		{
			List = List.Remove (item);
		}
	}
}
