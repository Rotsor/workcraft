package org.workcraft.plugins.mpsat.tasks;

import java.io.File;
import java.util.ArrayList;

import org.workcraft.plugins.mpsat.PunfUtilitySettings;
import org.workcraft.plugins.shared.tasks.ExternalProcessResult;
import org.workcraft.plugins.shared.tasks.ExternalProcessTask;
import org.workcraft.tasks.ProgressMonitor;
import org.workcraft.tasks.Result;
import org.workcraft.tasks.Task;
import org.workcraft.tasks.Result.Outcome;
import static org.workcraft.dependencymanager.advanced.core.GlobalCache.*;

public class PunfTask implements Task<ExternalProcessResult> {
	private String inputPath, outputPath;
	
	public PunfTask(String inputPath, String outputPath) 
	{
		this.inputPath = inputPath;
		this.outputPath = outputPath;
	}
	
	@Override
	public Result<? extends ExternalProcessResult> run(ProgressMonitor<? super ExternalProcessResult> monitor) 
	{
		ArrayList<String> command = new ArrayList<String>();
		command.add(eval(PunfUtilitySettings.punfCommand));
		
		for (String arg : eval(PunfUtilitySettings.punfArgs).split(" "))
			if (!arg.isEmpty())
				command.add(arg);
		
		command.add("-m="+outputPath);
		command.add(inputPath);

		Result<? extends ExternalProcessResult> res = new ExternalProcessTask(command, new File(".")).run(monitor);
		
		if (res.getOutcome() != Outcome.FINISHED)
			return res;
		
		ExternalProcessResult retVal = res.getReturnValue();
		if (retVal.getReturnCode() < 2)
			return Result.finished(retVal);
		else
			return Result.failed(retVal);
	}
}