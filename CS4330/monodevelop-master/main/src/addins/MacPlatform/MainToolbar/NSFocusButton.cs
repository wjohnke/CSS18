﻿//
// NSFocusButton.cs
//
// Author:
//       Dmytro Ovcharov <v-dmovch@microsoft.com>
//
// Copyright (c) 2015 Xamarin, Inc (http://www.xamarin.com)
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
using AppKit;
using System.Linq;

namespace MonoDevelop.MacIntegration.MainToolbar
{
	public class NSFocusButton: NSButton
	{
		public override void KeyDown (NSEvent theEvent)
		{
			var key = theEvent.Characters.FirstOrDefault ();
			var nextKeyView = GetNextFocusable (NextKeyView);
			if (nextKeyView != null && key == '\t') {
				Window.MakeFirstResponder (nextKeyView);
				return;
			}
			base.KeyDown (theEvent);
		}

		NSView GetNextFocusable(NSView view)
		{
			if (view == null || view is NSFocusButton || view is NSSearchField || view is NSPathControl)
				return view;
			
			return GetNextFocusable (view.NextKeyView);
		}
	}
}
