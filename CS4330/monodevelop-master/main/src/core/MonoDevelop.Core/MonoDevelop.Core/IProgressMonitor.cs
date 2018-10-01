//
// IProgressMonitor.cs
//
// Author:
//   Lluis Sanchez Gual
//
// Copyright (C) 2005 Novell, Inc (http://www.novell.com)
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
using System.Threading;
using System.Threading.Tasks;
using MonoDevelop.Core.ProgressMonitoring;

namespace MonoDevelop.Core
{
	public delegate void MonitorHandler (ProgressMonitor monitor);
	
	public interface IProgressMonitorFactory
	{
		ProgressMonitor CreateProgressMonitor ();
	}

	public class AsyncOperation
	{
		public static AsyncOperation CompleteOperation = new AsyncOperation (Task.FromResult(0), null);

		protected AsyncOperation ()
		{
			Task = Task.FromResult (0);
			CancellationTokenSource = new CancellationTokenSource ();
		}

		public AsyncOperation (Task task, CancellationTokenSource cancellationTokenSource)
		{
			Task = task;
			this.CancellationTokenSource = cancellationTokenSource ?? new CancellationTokenSource ();
		}

		public Task Task { get; protected set; }

		protected CancellationTokenSource CancellationTokenSource { get; set; }

		public bool IsCompleted {
			get { return Task.IsCompleted; }
		}

		public void Cancel ()
		{
			if (CancellationTokenSource != null)
				CancellationTokenSource.Cancel ();
		}
	}

	public class AsyncOperation<T>: AsyncOperation
	{
		public AsyncOperation (Task<T> task, CancellationTokenSource cancellationTokenSource): base (task, cancellationTokenSource)
		{
		}

		public new Task<T> Task {
			get { return (Task<T>) base.Task; }
		}
	}
}
