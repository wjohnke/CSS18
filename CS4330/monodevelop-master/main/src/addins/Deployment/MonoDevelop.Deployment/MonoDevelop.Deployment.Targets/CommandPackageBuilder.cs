//
// CommandPackageBuilder.cs
//
// Author:
//   Lluis Sanchez Gual
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
using System.IO;
using MonoDevelop.Projects;
using MonoDevelop.Core.Serialization;
using MonoDevelop.Core;
using MonoDevelop.Core.Execution;
using System.Threading;

namespace MonoDevelop.Deployment.Targets
{
	public class CommandPackageBuilder: PackageBuilder
	{
		string command;
		string args;
		bool externalConsole;
		bool closeConsoleWhenDone;
		string workingDirectory = ".";
		
		public override string Description {
			get { return GettextCatalog.GetString ("Execute command"); }
		}
		
		public override string Icon {
			get { return MonoDevelop.Ide.Gui.Stock.RunProgramIcon; }
		}
		
		[ItemProperty]
		public string Command {
			get { return command; }
			set { command = value; }
		}
		
		[ItemProperty]
		public string Arguments {
			get { return args; }
			set { args = value; }
		}
		
		[ItemProperty (DefaultValue=false)]
		public bool ExternalConsole {
			get { return externalConsole; }
			set { externalConsole = value; }
		}
		
		[ItemProperty (DefaultValue=false)]
		public bool CloseConsoleWhenDone {
			get { return closeConsoleWhenDone; }
			set { closeConsoleWhenDone = value; }
		}
		
		public override void CopyFrom (PackageBuilder other)
		{
			base.CopyFrom (other);
			CommandPackageBuilder t = other as CommandPackageBuilder;
			if (t != null) {
				command = t.command;
				args = t.args;
				externalConsole = t.externalConsole;
				closeConsoleWhenDone = t.closeConsoleWhenDone;
			}
		}
		
		protected override bool OnBuild (ProgressMonitor monitor, DeployContext ctx)
		{
			string consMsg;
			OperationConsole cons;
			if (ExternalConsole) {
				cons = ExternalConsoleFactory.Instance.CreateConsole (CloseConsoleWhenDone, monitor.CancellationToken);
				consMsg = GettextCatalog.GetString ("(in external terminal)");
			} else {
				cons = new MonitorConsole (monitor);
				consMsg = "";
			}
			
			monitor.Log.WriteLine (GettextCatalog.GetString ("Executing: {0} {1} {2}", Command, Arguments, consMsg));
			ProcessAsyncOperation process = Runtime.ProcessService.StartConsoleProcess (Command, Arguments, workingDirectory, cons);
			
			process.Task.Wait ();
			
			if (cons is MonitorConsole) {
				((MonitorConsole)cons).Dispose ();
			}
			return true;
		}
	}
	
	class MonitorConsole: OperationConsole
	{
		StringReader nullReader;
		ProgressMonitor monitor;
		CancellationTokenRegistration tr;
		
		public MonitorConsole (ProgressMonitor monitor)
		{
			this.monitor = monitor;
			tr = monitor.CancellationToken.Register (CancellationSource.Cancel);
		}
		
		public override void Dispose ()
		{
			tr.Dispose ();
		}
		
		public override TextReader In {
			get {
				if (nullReader == null)
					nullReader = new StringReader ("");
				return nullReader;
			}
		}
		
		public override TextWriter Out {
			get { return monitor.Log; }
		}
		
		public override TextWriter Error {
			get { return monitor.Log; }
		}
		
		public override TextWriter Log {
			get { return Out; }
		}
	}	
}
