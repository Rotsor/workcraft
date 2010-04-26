/**
 * 
 */
package org.workcraft.plugins.desij.tasks;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;

import org.workcraft.Framework;
import org.workcraft.dom.Model;
import org.workcraft.interop.Exporter;
import org.workcraft.serialisation.Format;
import org.workcraft.tasks.ProgressMonitor;
import org.workcraft.tasks.Result;
import org.workcraft.tasks.Task;
import org.workcraft.tasks.Result.Outcome;
import org.workcraft.util.Export;
import org.workcraft.util.Export.ExportTask;

/**
 * @author Dominic Wist
 *
 */
public class DesiJTask implements Task<DesiJResult> {
	
	private boolean userCancelled = false; // user cancelled the execution of desiJ
	private int returnCode = 0;
	
	private Model specModel;
	private File specificationFile; // specified in the last argument of desiJArgs
	private String[] desiJArgs; // parameters to call desiJMain(desijArgs);
		
	/*
	 * Constructor
	 */
	public DesiJTask(Model model, Framework framework, String[] desiJParameters) {
		
		this.specModel = model;		
		desiJArgs = new String[desiJParameters.length+1];
		
		// copy content from desiJParameters to desiJArgs
		for (int i=0; i < desiJParameters.length; i++)
			desiJArgs[i] = desiJParameters[i];
		
		this.specificationFile = getSpecificationFile(model, framework);
		// and add the specification filename as last argument
		try {
			desiJArgs[desiJParameters.length] = this.specificationFile.getCanonicalPath();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

		
	/* (non-Javadoc)
	 * @see org.workcraft.tasks.Task#run(org.workcraft.tasks.ProgressMonitor)
	 * 
	 */
	@Override
	public Result<DesiJResult> run(
			ProgressMonitor<DesiJResult> monitor) {
		
		// create desiJ thread
		DesiJThread desiJThread = new DesiJThread(desiJArgs);
		
		// start desiJ thread
		desiJThread.start();
				
		// monitor the desiJ execution
		while (true) {
			if (monitor.isCancelRequested() && desiJThread.isAlive()) {
				desiJThread.killThread(); // hard cancellation --> could lead to deadlocks 
				userCancelled = true;
			}
			if (!desiJThread.isAlive()) break;
			try {
				Thread.sleep(50);
			} catch (InterruptedException e) {
				desiJThread.killThread();
				userCancelled = true;
				break; 
			}
		}
		
		returnCode = desiJThread.getExitCode();
		
		if (userCancelled)
			return new Result<DesiJResult>(Outcome.CANCELLED); // maybe input or output files are not released
		
		// build DesiJResult
		DesiJResult result = new DesiJResult(this.specModel, this.specificationFile,
				getResultingComponents());
		
		if (returnCode < 2)
			return new Result<DesiJResult>(Outcome.FINISHED, result);
		else
			return new Result<DesiJResult>(Outcome.FAILED, result);
	}
	
	private File[] getResultingComponents() {
		
		String canonicalSpecName;
		try {
			canonicalSpecName = this.specificationFile.getCanonicalPath();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		
		// get WorkingDiretory and SpecFile names
		int fileSeparatorIndex = 
			canonicalSpecName.lastIndexOf(System.getProperty("file.separator"));
		final String workingDirName = canonicalSpecName.substring(0, fileSeparatorIndex);
		final String specFileName = canonicalSpecName.substring(fileSeparatorIndex+1);
		File workingDirectory = new File(workingDirName);
		
		// get all component filenames
		FilenameFilter filter = new FilenameFilter() { 
			public boolean accept(File dir, String name) { 
				return name.startsWith(specFileName + "__final_"); // current DesiJ naming convention
			} 
		}; 
		String[] children = workingDirectory.list(filter); // based on File names
		
		// format the result 
		File[] result = new File[children.length];
		if (children == null) { 
			// Either workingDirectory does not exist or is not a directory 
			return null;
		} 
		else { 
			for (int i=0; i<children.length; i++) {
				result[i] = new File(workingDirectory, children[i]);  
			} 
			return result;
		}
	}


	private File getSpecificationFile(Model model, Framework framework) {
		Exporter stgExporter = Export.chooseBestExporter(framework.getPluginManager(), model, Format.STG);
		
		if (stgExporter == null)
			throw new RuntimeException ("Exporter not available: model class " + model.getClass().getName() + " to format STG.");
		
		File stgFile;
		
		try {
			stgFile = File.createTempFile("specification", stgExporter.getExtenstion());
			ExportTask exportTask = new ExportTask(stgExporter, model, stgFile.getCanonicalPath());
			framework.getTaskManager().execute(exportTask, "Exporting .g");
			
			return stgFile;
			
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

}
