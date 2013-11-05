Workflow
======
R codes are broken down into 4 main files: load, clean, func, do.
- "load.R" contains the code to load all the data and R packages to do the analysis.
- "clean.R" prepares the loaded data for analysis. 
- "func.R" contains the functions and methods for analysis, visualization, and reporting.
- "do.R" is the GUI, where the code is broken down into sections based on the analysis workflow. 
- For example, if the desired analysis is prediction, GUI has a section called prediction which serves as a one-stop shop to set the options, run the prediction function, and get the reports.

The user starts with loading the files "load.R", "clean.R", and "func.R" and then chooses the analysis of interest from "do.R".

