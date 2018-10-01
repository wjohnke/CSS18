﻿//
// SelectionSurroundingProviderWrapper.cs
//
// Author:
//       Mike Krüger <mkrueger@xamarin.com>
//
// Copyright (c) 2014 Xamarin Inc. (http://xamarin.com)
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
using Mono.TextEditor;
using MonoDevelop.SourceEditor.Wrappers;

namespace MonoDevelop.SourceEditor.Wrappers
{
	class SelectionSurroundingProviderWrapper : ISelectionSurroundingProvider
	{
		readonly MonoDevelop.Ide.Editor.Extension.SelectionSurroundingProvider surroundingProvider;

		public SelectionSurroundingProviderWrapper (MonoDevelop.Ide.Editor.Extension.SelectionSurroundingProvider surroundingProvider)
		{
			if (surroundingProvider == null)
				throw new ArgumentNullException ("surroundingProvider");
			this.surroundingProvider = surroundingProvider;
		}

		#region ISelectionSurroundingProvider implementation

		bool ISelectionSurroundingProvider.GetSelectionSurroundings (TextEditorData textEditorData, uint unicodeKey, out string start, out string end)
		{
			return surroundingProvider.GetSelectionSurroundings (unicodeKey, out start, out end);
		}

		void ISelectionSurroundingProvider.HandleSpecialSelectionKey (TextEditorData textEditorData, uint unicodeKey)
		{
			surroundingProvider.HandleSpecialSelectionKey (unicodeKey);
		}

		#endregion
	}
}

