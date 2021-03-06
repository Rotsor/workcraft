package org.workcraft.plugins.mpsat.gui;

import info.clearthought.layout.TableLayout;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import org.workcraft.Trace;
import org.workcraft.exceptions.NotImplementedException;
import org.workcraft.gui.MainWindow;
import org.workcraft.gui.ToolboxPanel;
import org.workcraft.gui.graph.GraphEditorPanel;
import org.workcraft.interop.ServiceNotAvailableException;
import org.workcraft.workspace.WorkspaceEntry;


@SuppressWarnings("serial")
public class SolutionPanel extends JPanel {
	private JPanel buttonsPanel;
	private JTextArea traceText;
	
	public SolutionPanel(final MainWindow mainWindow, final WorkspaceEntry we, final Trace t, final ActionListener closeAction) throws ServiceNotAvailableException {
		super (new TableLayout(new double[][]
		        { { TableLayout.FILL, TableLayout.PREFERRED },
				{TableLayout.FILL} }
		));
		
		traceText = new JTextArea();
		traceText.setText(t.toString());
		
		final JScrollPane scrollPane = new JScrollPane();
		scrollPane.setViewportView(traceText);
		
		buttonsPanel = new JPanel();
		buttonsPanel.setLayout(new BoxLayout(buttonsPanel, BoxLayout.Y_AXIS));
		
		JButton saveButton = new JButton("Save");		
		
		JButton playButton = new JButton("Play trace");
		playButton.addActionListener(new ActionListener()
		{
			@Override
			public void actionPerformed(ActionEvent e) {
				GraphEditorPanel currentEditor = mainWindow.getCurrentEditor();
				if(currentEditor == null || currentEditor.getWorkspaceEntry() != we)
				{
					final List<GraphEditorPanel> editors = mainWindow.getEditors(we);
					if(editors.size()>0) {
						currentEditor = editors.get(0);
						mainWindow.requestFocus(currentEditor);
					}
					else {
						try {
							currentEditor = mainWindow.createEditorWindow(we);
						} catch (ServiceNotAvailableException e1) {
							throw new NotImplementedException("TODO: ensure this does not happen with type system");
						}
					}
				}
				if(true)throw new NotImplementedException("Todo: set trace");
				/*final ToolboxPanel toolbox = currentEditor.getToolBox();
				final SimulationTool tool = toolbox.getToolInstance(SimulationTool.class);
				tool.setTrace(t);
				toolbox.selectTool(tool);*/
				closeAction.actionPerformed(null);
			}
		});
		
		buttonsPanel.add(saveButton);
		buttonsPanel.add(playButton);
		
		
		add(scrollPane, "0 0");
		add(buttonsPanel, "1 0");
	}
}
