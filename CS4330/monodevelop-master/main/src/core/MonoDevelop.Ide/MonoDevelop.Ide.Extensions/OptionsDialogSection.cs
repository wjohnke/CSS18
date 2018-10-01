// OptionsDialogSection.cs
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
using Mono.Addins;
using MonoDevelop.Core;
using MonoDevelop.Components;

namespace MonoDevelop.Ide.Extensions
{
	[ExtensionNodeChild (typeof(OptionsDialogSection))]
	[ExtensionNodeChild (typeof(OptionsPanelNode))]
	[ExtensionNode ("Section")]
	class OptionsDialogSection: OptionsPanelNode, ICloneable
	{
		[NodeAttribute]
		string icon;
		
		[NodeAttribute]
		string headerImageResource;

		[NodeAttribute]
		string headerFillerImageResource;

		public OptionsDialogSection ()
		{
		}
		
		public OptionsDialogSection (Type panelType): base (panelType)
		{
		}
		
		public IconId Icon {
			get {
				return icon;
			}
			set {
				icon = value;
			}
		}

		public Xwt.Drawing.Image HeaderImage { get; set; }

		public Xwt.Drawing.Image HeaderFillerImageResource { get; set; }

		protected override void Read (NodeElement elem)
		{
			base.Read (elem);
			if (headerImageResource != null)
				HeaderImage = Addin.GetImageResource (headerImageResource);
			if (headerFillerImageResource != null)
				HeaderFillerImageResource = Addin.GetImageResource (headerFillerImageResource);
		}
		
		public object Clone ()
		{
			return MemberwiseClone ();
		}
	}
}
