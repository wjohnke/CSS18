//
// ExternalToolService.cs
//
// Author:
//   Mike Krüger <mkrueger@novell.com>
//
// Copyright (C) 2007 Novell, Inc (http://www.novell.com)
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
using System.Diagnostics;
using System.IO;
using System.Collections.Generic;
using System.Xml;

using MonoDevelop.Core;
using MonoDevelop.Components.Commands;
using MonoDevelop.Ide.Commands;

namespace MonoDevelop.Ide.ExternalTools
{
	public static class ExternalToolService
	{
		static string FileName = "MonoDevelop-tools.xml";
		static string Version  = "2.0";
		
		static List<ExternalTool> tools;
		
		public static List<ExternalTool> Tools {
			get {
				return tools;
			}
			set {
				UnregisterCommands ();
				tools = value;
				RegisterCommands ();
			}
		}

		static void UnregisterCommands ()
		{
			if (tools == null)
				return;
			for (int i = 0; i < tools.Count; i++) {
				var cmd = IdeApp.CommandService.GetActionCommand ("MonoDevelop.CustomCommands.Command" + i);
				if (cmd != null)
					IdeApp.CommandService.UnregisterCommand (cmd);
			}
		}

		static void RegisterCommands ()
		{
			if (tools == null)
				return;
			for (int i = 0; i < tools.Count; i++) {
				var tool = tools [i];
				ActionCommand cmd = new ActionCommand ("MonoDevelop.CustomCommands.Command" + i, tool.MenuCommand, null);
				cmd.DefaultHandler = new RunCustomToolHandler (tool);
				cmd.Category = GettextCatalog.GetString ("Tools (Custom)");
				cmd.Description = GettextCatalog.GetString ("Start tool {0}", string.Join (string.Empty, tool.MenuCommand.Split ('&')));
				cmd.AccelKey = tool.AccelKey;
				IdeApp.CommandService.RegisterCommand (cmd);
			}
		}
		
		static ExternalToolService ()
		{
			try {
				Tools = LoadTools ();
			} catch (Exception e) {
				LoggingService.LogError ("ExternalToolService: Exception while loading tools.", e);
				Tools = new List<ExternalTool> ();
			}
		}
		
#region I/O
		const string Node             = "Tools";
		const string VersionAttribute = "version";
		
		static void SaveTools (string fileName)
		{
			XmlWriterSettings settings = new XmlWriterSettings ();
			settings.Encoding = System.Text.Encoding.UTF8;
			settings.Indent = true;
			XmlWriter writer = XmlTextWriter.Create (fileName, settings);
			try {
				writer.WriteStartDocument ();
				writer.WriteStartElement (Node);
				writer.WriteAttributeString (VersionAttribute, Version);
			
				foreach (ExternalTool tool in tools)
					tool.Write (writer);
				
				writer.WriteEndElement (); // Node 
			} finally {
				writer.Close ();
			}
		}
		
		public static void SaveTools ()
		{
			SaveTools (UserProfile.Current.ConfigDir.Combine (FileName));
		}
		
		static List<ExternalTool> LoadTools (string fileName)
		{
			if (!File.Exists (fileName))
				return null;
			List<ExternalTool> result = new List<ExternalTool> ();
			XmlReader reader = XmlTextReader.Create (fileName);
			try {
				while (reader.Read ()) {
					if (reader.IsStartElement ()) {
						switch (reader.LocalName) {
						case Node:
							string fileVersion = reader.GetAttribute (VersionAttribute);
							if (fileVersion != Version) 
								return null;
							break;
						case ExternalTool.Node:
							result.Add (ExternalTool.Read (reader));
							break;
						}
					}
				}
			} finally {
				reader.Close ();
			}
			return result;
		}
		
		static List<ExternalTool> LoadTools ()
		{
			List<ExternalTool> result = LoadTools (UserProfile.Current.ConfigDir.Combine (FileName));
			if (result == null) {
				LoggingService.LogInfo ("ExternalToolService: No user templates, reading default templates.");
				result = LoadTools (Path.Combine (Path.Combine (PropertyService.DataPath, "options"), FileName));
			}
			
			if (result == null)
				return new List<ExternalTool> ();
			
			return result;
		}
#endregion		
	}
}
