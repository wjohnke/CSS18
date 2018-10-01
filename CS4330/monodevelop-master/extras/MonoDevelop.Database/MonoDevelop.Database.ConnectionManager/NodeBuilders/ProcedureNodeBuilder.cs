//
// Authors:
//   Christian Hergert	<chris@mosaix.net>
//   Ben Motmans  <ben.motmans@gmail.com>
//
// Copyright (C) 2005 Mosaix Communications, Inc.
// Copyright (c) 2007 Ben Motmans
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
using System.Threading;
using System.Collections.Generic;
using MonoDevelop.Database.Sql;
using MonoDevelop.Core;
using MonoDevelop.Ide;
using MonoDevelop.Ide.Gui.Pads;
using MonoDevelop.Components.Commands;
using MonoDevelop.Ide.Gui.Components;

namespace MonoDevelop.Database.ConnectionManager
{
	public class ProcedureNodeBuilder : TypeNodeBuilder
	{
		public ProcedureNodeBuilder ()
			: base ()
		{
		}
		
		public override Type NodeDataType {
			get { return typeof (ProcedureNode); }
		}
		
		public override string ContextMenuAddinPath {
			get { return "/MonoDevelop/Database/ContextMenu/ConnectionManagerPad/ProcedureNode"; }
		}
		
		public override Type CommandHandlerType {
			get { return typeof (ProcedureNodeCommandHandler); }
		}
		
		public override void GetNodeAttributes (ITreeNavigator treeNavigator, object dataObject, ref NodeAttributes attributes)
		{
			attributes |= NodeAttributes.AllowRename;
		}		
		
		public override string GetNodeName (ITreeNavigator thisNode, object dataObject)
		{
			ProcedureNode node = dataObject as ProcedureNode;
			return node.Procedure.Name;
		}
		
		public override void BuildNode (ITreeBuilder builder, object dataObject, NodeInfo nodeInfo)
		{
			ProcedureNode node = dataObject as ProcedureNode;
			
			nodeInfo.Label = node.Procedure.Name;
			if (node.Procedure.IsFunction)
				nodeInfo.Icon = Context.GetIcon ("md-db-function");
			else
				nodeInfo.Icon = Context.GetIcon ("md-db-procedure");
		}
		
		public override void BuildChildNodes (ITreeBuilder builder, object dataObject)
		{
			ThreadPool.QueueUserWorkItem (new WaitCallback (BuildChildNodesThreaded), dataObject);
		}
		
		private void BuildChildNodesThreaded (object state)
		{
			ProcedureNode node = state as ProcedureNode;
			ITreeBuilder builder = Context.GetTreeBuilder (state);
			ISchemaProvider provider = node.ConnectionContext.SchemaProvider;

			if (provider.IsSchemaActionSupported (SchemaType.Constraint, SchemaActions.Schema))
				DispatchService.GuiDispatch (delegate {
					builder.AddChild (new ParametersNode (node.ConnectionContext, node.Procedure));
					builder.Expanded = true;
				});
		}
		
		public override bool HasChildNodes (ITreeBuilder builder, object dataObject)
		{
			ProcedureNode node = dataObject as ProcedureNode;
			return node.ConnectionContext.SchemaProvider.IsSchemaActionSupported (SchemaType.ProcedureParameter, SchemaActions.Schema);
		}
		
	}
	
	public class ProcedureNodeCommandHandler : NodeCommandHandler
	{
		public override DragOperation CanDragNode ()
		{
			return DragOperation.None;
		}
		
		public override void RenameItem (string newName)
		{
			ProcedureNode node = (ProcedureNode)CurrentNode.DataItem;
			if (node.Procedure.Name != newName)
				ThreadPool.QueueUserWorkItem (new WaitCallback (RenameItemThreaded), new object[]{ node, newName });
		}
		
		private void RenameItemThreaded (object state)
		{
			object[] objs = state as object[];
			
			ProcedureNode node = objs[0] as ProcedureNode;
			string newName = objs[1] as string;
			IEditSchemaProvider provider = (IEditSchemaProvider)node.Procedure.SchemaProvider;
			
			if (provider.IsValidName (newName)) {
				provider.RenameProcedure (node.Procedure, newName);
				node.Refresh ();
			} else {
				DispatchService.GuiDispatch (delegate () {
					MessageService.ShowError (String.Format (
						"Unable to rename procedure '{0}' to '{1}'!",
						node.Procedure.Name, newName
					));
				});
			}
			node.Refresh ();
		}
		
		protected void OnRefreshParent ()
		{
			if (CurrentNode.MoveToParent ()) {
				BaseNode node = CurrentNode.DataItem as BaseNode;
				node.Refresh ();
			}
		}
		
		[CommandHandler (ConnectionManagerCommands.AlterProcedure)]
		protected void OnAlterProcedure ()
		{
			ProcedureNode node = CurrentNode.DataItem as ProcedureNode;
			IDbFactory fac = node.ConnectionContext.DbFactory;
			IEditSchemaProvider schemaProvider = (IEditSchemaProvider)node.ConnectionContext.SchemaProvider;
			ProcedureSchema alterProc = (ProcedureSchema)node.Procedure.Clone ();
			if (fac.GuiProvider.ShowProcedureEditorDialog (schemaProvider, alterProc, false))
				ThreadPool.QueueUserWorkItem (new WaitCallback (OnAlterProcedureThreaded), alterProc);
		}
		
		private void OnAlterProcedureThreaded (object state)
		{
			ProcedureNode node = CurrentNode.DataItem as ProcedureNode;
			IDbFactory fac = node.ConnectionContext.DbFactory;
			IEditSchemaProvider schemaProvider = (IEditSchemaProvider)node.ConnectionContext.SchemaProvider;
			ProcedureAlterSchema alterSchema = new ProcedureAlterSchema (node.Procedure, (ProcedureSchema)((ProcedureSchema)state).Clone ());
			schemaProvider.AlterProcedure (alterSchema);
			OnRefreshParent ();
		}
		
		[CommandHandler (ConnectionManagerCommands.DropProcedure)]
		protected void OnDropProcedure ()
		{
			ProcedureNode node = (ProcedureNode)CurrentNode.DataItem;
			AlertButton dropButton = new AlertButton (AddinCatalog.GetString ("Drop"), Gtk.Stock.Delete);
			if (MessageService.Confirm (
				AddinCatalog.GetString ("Are you sure you want to drop procedure '{0}'", node.Procedure.Name),
				dropButton
			)) {
				ThreadPool.QueueUserWorkItem (new WaitCallback (OnDropProcedureThreaded), CurrentNode.DataItem);
			}
		}
			
		private void OnDropProcedureThreaded (object state)
		{
			ProcedureNode node = (ProcedureNode)state;
			IEditSchemaProvider provider = (IEditSchemaProvider)node.ConnectionContext.SchemaProvider;
			
			provider.DropProcedure (node.Procedure);
			OnRefreshParent ();
		}
		
		[CommandHandler (MonoDevelop.Ide.Commands.EditCommands.Rename)]
		protected void OnRenameProcedure ()
		{
			Tree.StartLabelEdit ();
		}
		
		[CommandUpdateHandler (ConnectionManagerCommands.DropProcedure)]
		protected void OnUpdateDropProcedure (CommandInfo info)
		{
			BaseNode node = (BaseNode)CurrentNode.DataItem;
			//info.Enabled = node.ConnectionContext.DbFactory.IsActionSupported ("Procedure", SchemaActions.Drop);
		}
		
		[CommandUpdateHandler (MonoDevelop.Ide.Commands.EditCommands.Rename)]
		protected void OnUpdateRenameProcedure (CommandInfo info)
		{
			BaseNode node = (BaseNode)CurrentNode.DataItem;
			//info.Enabled = node.ConnectionContext.DbFactory.IsActionSupported ("Procedure", SchemaActions.Rename);
		}
		
		[CommandUpdateHandler (ConnectionManagerCommands.AlterProcedure)]
		protected void OnUpdateAlterProcedure (CommandInfo info)
		{
			BaseNode node = (BaseNode)CurrentNode.DataItem;
			//info.Enabled = node.ConnectionContext.DbFactory.IsActionSupported ("Procedure", SchemaActions.Alter);
		}
	}
}
