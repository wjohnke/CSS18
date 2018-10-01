// 
// ResultsEditorExtension.cs
//  
// Author:
//       Michael Hutchinson <mhutchinson@novell.com>
// 
// Copyright (c) 2010 Novell, Inc.
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

using System.Collections.Generic;
using MonoDevelop.Core.Text;
using MonoDevelop.Ide.Editor;

namespace MonoDevelop.AnalysisCore.Gui
{
/*
	class ResultMarker : UnderlineTextSegmentMarker
	{
		readonly Result result;

		public ResultMarker (Result result, TextSegment segment) : base ("", segment)
		{
			this.result = result;
		}
		
		public Result Result { get { return result; } }
		
		//utility for debugging
		public string Message { get { return result.Message; } }
		
		static Cairo.Color GetColor (TextEditor editor, Result result)
		{
			switch (result.Level) {
			case Severity.None:
				return editor.ColorStyle.PlainText.Background;
			case Severity.Error:
				return editor.ColorStyle.UnderlineError.Color;
			case Severity.Warning:
				return editor.ColorStyle.UnderlineWarning.Color;
			case Severity.Suggestion:
				return editor.ColorStyle.UnderlineSuggestion.Color;
			case Severity.Hint:
				return editor.ColorStyle.UnderlineHint.Color;
			default:
				throw new System.ArgumentOutOfRangeException ();
			}
		}
		
		public override void Draw (TextEditor editor, Cairo.Context cr, Pango.Layout layout, bool selected, int startOffset, int endOffset, double y, double startXPos, double endXPos)
		{
			if (Debugger.DebuggingService.IsDebugging)
				return;
			int markerStart = Segment.Offset;
			int markerEnd = Segment.EndOffset;
			if (markerEnd < startOffset || markerStart > endOffset) 
				return;
			
			bool drawOverlay = result.InspectionMark == IssueMarker.GrayOut;
			
			if (drawOverlay && editor.IsSomethingSelected) {
				var selectionRange = editor.SelectionRange;
				if (selectionRange.Contains (markerStart) && selectionRange.Contains (markerEnd))
					return;
				if (selectionRange.Contains (markerEnd))
					markerEnd = selectionRange.Offset;
				if (selectionRange.Contains (markerStart))
					markerStart = selectionRange.EndOffset;
				if (markerEnd <= markerStart)
					return;
			}
			
			double drawFrom;
			double drawTo;
				
			if (markerStart < startOffset && endOffset < markerEnd) {
				drawTo = endXPos;
				var line = editor.GetLineByOffset (startOffset);
				int offset = line.GetIndentation (editor.Document).Length;
				drawFrom = startXPos + (layout.IndexToPos (offset).X  / Pango.Scale.PangoScale);
			} else {
				int start;
				if (startOffset < markerStart) {
					start = markerStart;
				} else {
					var line = editor.GetLineByOffset (startOffset);
					int offset = line.GetIndentation (editor.Document).Length;
					start = startOffset + offset;
				}
				int end = endOffset < markerEnd ? endOffset : markerEnd;
				int x_pos;

				x_pos = layout.IndexToPos (start - startOffset).X;
				drawFrom = startXPos + (int)(x_pos / Pango.Scale.PangoScale);
				x_pos = layout.IndexToPos (end - startOffset).X;
	
				drawTo = startXPos + (int)(x_pos / Pango.Scale.PangoScale);
			}
			
			drawFrom = System.Math.Max (drawFrom, editor.TextViewMargin.XOffset);
			drawTo = System.Math.Max (drawTo, editor.TextViewMargin.XOffset);
			if (drawFrom >= drawTo)
				return;
			
			double height = editor.LineHeight / 5;
			cr.SetSourceColor (GetColor (editor, Result));
			if (drawOverlay) {
				cr.Rectangle (drawFrom, y, drawTo - drawFrom, editor.LineHeight);
				var color = editor.ColorStyle.PlainText.Background;
				color.A = 0.6;
				cr.SetSourceColor (color);
				cr.Fill ();
			} else if (result.InspectionMark == IssueMarker.WavedLine) {	
				Pango.CairoHelper.ShowErrorUnderline (cr, drawFrom, y + editor.LineHeight - height, drawTo - drawFrom, height);
			} else if (result.InspectionMark == IssueMarker.DottedLine) {
				cr.Save ();
				cr.LineWidth = 1;
				cr.MoveTo (drawFrom + 1, y + editor.LineHeight - 1 + 0.5);
				cr.RelLineTo (System.Math.Min (drawTo - drawFrom, 4 * 3), 0);
				cr.SetDash (new double[] { 2, 2 }, 0);
				cr.Stroke ();
				cr.Restore ();
			} else {
				cr.MoveTo (drawFrom, y + editor.LineHeight - 1);
				cr.LineTo (drawTo, y + editor.LineHeight - 1);
				cr.Stroke ();
			}
		}
	}

	class GrayOutMarker : ResultMarker, IChunkMarker
	{
		public GrayOutMarker (Result result, TextSegment segment) : base (result, segment)
		{
		}

		public override void Draw (TextEditor editor, Cairo.Context cr, Pango.Layout layout, bool selected, int startOffset, int endOffset, double y, double startXPos, double endXPos)
		{
		}

		#region IChunkMarker implementation

		void IChunkMarker.TransformChunks (List<Chunk> chunks)
		{
			int markerStart = Segment.Offset;
			int markerEnd = Segment.EndOffset;
			for (int i = 0; i < chunks.Count; i++) {
				var chunk = chunks [i];
				if (chunk.EndOffset < markerStart || markerEnd <= chunk.Offset)
					continue;
				if (chunk.Offset == markerStart && chunk.EndOffset == markerEnd)
					return;
				
				if (chunk.Offset <= markerStart && chunk.EndOffset >= markerEnd) {
					if (markerStart - chunk.Offset > 0) {
						var newChunk = new Chunk (chunk.Offset, markerStart - chunk.Offset, chunk.Style);
						chunks.Insert (i, newChunk);
						chunk.Offset += newChunk.Length;
						chunk.Length -= newChunk.Length;
						chunk = newChunk;
					}
					if (markerEnd < chunk.EndOffset) {
						var newChunk = new Chunk (chunk.Offset, markerEnd - chunk.Offset, chunk.Style);
						chunks.Insert (i, newChunk);
						chunk.Offset += newChunk.Length;
						chunk.Length -= newChunk.Length;
					}
					continue;
				}
			}
		}

		void IChunkMarker.ChangeForeColor (TextEditor editor, Chunk chunk, ref Cairo.Color color)
		{
			if (Debugger.DebuggingService.IsDebugging)
				return;
			int markerStart = Segment.Offset;
			int markerEnd = Segment.EndOffset;
			if (chunk.EndOffset <= markerStart || markerEnd <= chunk.Offset) 
				return;
			var bgc = editor.ColorStyle.PlainText.Background;
			double alpha = 0.6;
			color = new Cairo.Color (
				color.R * alpha + bgc.R * (1.0 - alpha),
				color.G * alpha + bgc.G * (1.0 - alpha),
				color.B * alpha + bgc.B * (1.0 - alpha)
			);
		}
		#endregion
	}
*/
}
