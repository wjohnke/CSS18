//
// MdTextViewLineCollection.cs
//
// Author:
//       Mike Krüger <mikkrg@microsoft.com>
//
// Copyright (c) 2018 Microsoft Corporation. All rights reserved.
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
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Formatting;
using MonoDevelop.Core.Text;

namespace Mono.TextEditor
{
	partial class MdTextViewLineCollection : List<ITextViewLine>, ITextViewLineCollection
	{
		readonly MonoTextEditor textEditor;
		readonly ITextSourceVersion version;

		public MdTextViewLineCollection (MonoTextEditor textEditor) : base (64)
		{
			this.textEditor = textEditor;
			this.version = this.textEditor.Document.Version;
		}

		internal void Add (int logicalLineNumber, DocumentLine line)
		{
			if (line == null)
				return;
			var newLine = new MdTextViewLine (this, textEditor, line, logicalLineNumber, textEditor.TextViewMargin.GetLayout (line));
			for (int i = 0; i < Count; i++) {
				if (((MdTextViewLine)this [i]).LineNumber == logicalLineNumber) {
					this [i] = newLine;
					return;
				}
			}
			Add (newLine);
		}

		internal void RemoveLinesBefore (int lineNumber)
		{
			for (int i = this.Count - 1; i >= 0; i--) {
				if (((MdTextViewLine)this [i]).LineNumber < lineNumber) {
					this.RemoveAt (i);
				}
			}
		}

		internal void RemoveLinesAfter (int lineNumber)
		{
			for (int i = this.Count - 1; i >= 0; i--) {
				if (((MdTextViewLine)this [i]).LineNumber > lineNumber) {
					this.RemoveAt (i);
				}
			}
		}

		public ITextViewLine FirstVisibleLine => this.FirstOrDefault ();

		public ITextViewLine LastVisibleLine => this.LastOrDefault ();

		public SnapshotSpan FormattedSpan => new SnapshotSpan (this [0].Start, this.Last ().EndIncludingLineBreak);

		public bool IsValid => version.CompareAge (textEditor.Document.Version) == 0;

		public bool ContainsBufferPosition (SnapshotPoint bufferPosition)
		{
			foreach (var line in this) {
				if (line.Start <= bufferPosition && line.End < bufferPosition)
					return true;
			}
			return false;
		}

		public TextBounds GetCharacterBounds (SnapshotPoint bufferPosition)
		{
			foreach (var line in this) {
				if (line.Start <= bufferPosition && line.End < bufferPosition) {
					// TODO: correct both 0 parameters
					return new TextBounds (0, line.Top, 0, line.Height, line.TextTop, line.TextHeight);
				}
			}

			return new TextBounds ();
		}

		public int GetIndexOfTextLine (ITextViewLine textLine)
		{
			return IndexOf (textLine);
		}

		public Collection<TextBounds> GetNormalizedTextBounds (SnapshotSpan bufferSpan)
		{
			var bounds = new Collection<TextBounds> ();
			foreach (var line in this)
				foreach (var bound in line.GetNormalizedTextBounds (bufferSpan))
					bounds.Add (bound);
			return bounds;
		}

		public SnapshotSpan GetTextElementSpan (SnapshotPoint bufferPosition)
		{
			return new SnapshotSpan (bufferPosition, 1);
		}

		public ITextViewLine GetTextViewLineContainingBufferPosition (SnapshotPoint bufferPosition)
		{
			return this.FirstOrDefault (l => l.ContainsBufferPosition (bufferPosition));
		}

		public ITextViewLine GetTextViewLineContainingYCoordinate (double y)
		{
			return this.FirstOrDefault (l => l.Top <= y && l.Top + l.Height >= y);
		}

		public Collection<ITextViewLine> GetTextViewLinesIntersectingSpan (SnapshotSpan bufferSpan)
		{
			var result = new Collection<ITextViewLine> ();
			foreach (var line in this)
				if (line.IntersectsBufferSpan (bufferSpan))
					result.Add (line);
			return result;
		}

		public bool IntersectsBufferSpan (SnapshotSpan bufferSpan)
		{
			foreach (var line in this)
				if (line.IntersectsBufferSpan (bufferSpan))
					return true;
			return false;
		}
	}
}
