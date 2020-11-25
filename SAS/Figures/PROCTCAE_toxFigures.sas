
 /*-------------------------------------------------------------------------------------------*
   | MACRO NAME	 :	PROCTCAE_toxFigures
   | VERSION	 :	1.0.2
   | SHORT DESC  :	Creates PRO-CTCAE severity frequency distribution figures for individual 
   |			  	survey items and composite grades
   |	
   *------------------------------------------------------------------------------------------*
   | AUTHORS  	 :	Blake T Langlais, Amylou C Dueck 
   *------------------------------------------------------------------------------------------*
   | 
   *------------------------------------------------------------------------------------------*
   | PURPOSE	 :	This macro takes in a SAS data set with numeric PRO-CTCAE survey variables 
   |				then create severity frequency distribution figures for individual and 
   |				composite grades, at each time point as well as for the maximum post baseline
   |				and baseline adjusted scores. These figures can be outputted as individual JPEG 
   |				image files to a user-specified directory.
   |				   
   |			   	PRO-CTCAE variable names MUST conform to a pre-specified naming structure. PRO-CTCAE 
   |			   	variable names are made up of FOUR components: 1)'PROCTCAE', 2) number [1,2,3, ..., i, ..., 80], 
   |               	3) 'A', 'B', or 'C' component of the i-th PRO-CTCAE field, 4) and 'SCL' (if severity,
   |               	interference, or frequency) or 'IND' (if yes/no variable). Each component
   |               	must be delimitated by an underscore (_) 
   |             		EX1: Question 1 of PRO-CTCAE should be: PROCTCAE_1A_SCL
   |             		EX2: Question 48 of PRO-CTCAE should be: PROCTCAE_48A_SCL, PROCTCAE_48B_SCL, PROCTCAE_48C_SCL
   |			
   |				Similarly, composite grade variable names are expected to be named as 'PROCTCAE', then the
   |				survey item number, followed by 'COMP'. Again seperated by an underscore (_).
   |					EX1: Question 8 composite grade should be named as PROCTCAE_8_COMP
   |					EX2: Question 48 composite grade should be named as PROCTCAE_48_COMP
   |
   |				To help with this naming structure, a reference SAS dataset with these variable names and 
   |				their respective labels can be produced here if the user runs this macro with no specified 
   |				parameters. The SAS dataset created is named PROCTCAE_table. For this, just run the code below:
   |					%PROCTCAE_toxFigures;
   |
   |				PRO-CTCAE severity, interference, frequency items and subsequent composite grades
   |				are used to construct these figures. Survey items with yes/no responses are not.
   |				See available accompanying SAS macro for more on constructing composite grades (PROCTCAE_scores).
   |
   |				EXTPECTED DATA FORMAT
   |				 Data format should be in 'long' format, where each row/record/observation reflects
   |				 a unique visit/cycle/time point for an individual and each PRO-CTCAE item is a variable/column.   
   |
   |				ACKNOWLEDGEMENTS
   |				 Special thanks to Allison Deal, Carolyn Mead-Harvey, Gina Mazza, and Paul Novotny for their
   |				 help in testing and feature recommendation. 
   |
   |				
   |				[-]	https://healthcaredelivery.cancer.gov/pro-ctcae/pro-ctcae_english.pdf
   |				[-] Ethan Basch, et al. Development of a Composite Grading Algorithm for the 
   |					National Cancer Institute’s Patient-Reported Outcomes version of the Common 
   |					Terminology Criteria for Adverse Events (PRO-CTCAE). ISOQOL 2019.
   |				[-] Basch E, et al. Composite Grading Algorithm for the National Cancer Institute’s 
   |					Patient-Reported Outcomes version of the Common Terminology Criteria for Adverse 
   |					Events (PRO-CTCAE). Clinical Trials 2020.
   |
   *------------------------------------------------------------------------------------------*
   | OPERATING SYSTEM COMPATIBILITY
   |
   | UNIX SAS v8   :
   | UNIX SAS v9   :   YES
   | MVS SAS v8    :
   | MVS SAS v9    :
   | PC SAS v8     :
   | PC SAS v9     :   YES
   *------------------------------------------------------------------------------------------*
   | MACRO CALL
   |
   
   	* -- Required parameters;
	%PROCTCAE_toxFigures(dsn= , 
						 id_var = , 
						 cycle_var = ,
						 baseline_val = );
	
   |
   *------------------------------------------------------------------------------------------*
   | REQUIRED PARAMETERS
   |
   | Name      : dsn
   | Type      : SAS data set name
   | Purpose   : Data set with PRO-CTCAE items and row ID (with optional cycle and arm fields)
   |
   | Name      : id_var
   | Type      : Valid variable name
   | Purpose   : Field name of ID variable differentiating each PRO-CTCAE survey
   |
   | Name      : cycle_var
   | Type      : Valid variable name (must be numeric vaiable)
   | Purpose   : Variable differentiating one longitudinal/repeated PRO-CTCAE survey
   |             from another, within an individual ID. Figure labeling will inherit
   |			 the variable's formating.
   |    
   | Name      : baseline_val
   | Type      : Numerical value of baseline cycle/time point within the cycle_var variable
   | Purpose   : This is the value indicating an individual's baseline (or first) time 
   |			 point (e.g. time 0, cycle 1, visit 1, would be 0, 1, 1, respectively) 
   |  
   *------------------------------------------------------------------------------------------*
   | OPTIONAL PARAMETERS
   |
   | Name      : arm_var
   | Type      : Valid variable name (must be a character variable)
   | Purpose   : Field name of arm variable differentiating treatment groups
   | Default   : Overall frequencies will be reported (if no arm/grouping variable is provided)
   |
   | Name      : cycle_fmt
   | Type      : Valid SAS format name (single)
   | Purpose   : Name of the SAS format to be applied to the numeric cycle variable. Where
   |			 the baseline cycle (or first time point) is equal to 0.
   |    
   | Name      : cycle_limit
   | Type      : Numeric
   | Purpose   : Limit the data to be analyzed up to and including a given cycle number
   | Default   : All available cycle time points are used for calculation
   |
   | Name      : plot_limit
   | Type      : Numeric
   | Purpose   : Limit the number of cycles to be plotted up to and including a given cycle number
   | Default   : All available cycle time points are plotted
   |
   | Name      : height
   | Type      : Numeric
   | Purpose   : Specify the figure height in inches
   | Default   : 6.2
   |
   | Name      : width
   | Type      : Numeric
   | Purpose   : Specify the figure width in inches
   | Default   : 10
   |
   | Name      : label
   | Type      : 0 = no frequency bar labels
   |			 1 = sample size (n) within each cycle (symptom grade 0 or higher)
   |			 2 = sample size (n) within each cycle with present symptoms (symptom grade > 0)
   |			 3 = sample size (n) within each cycle with severe symptoms (symptom grade >= 3)
   |			 4 = percent of subjects (%) within each cycle with present symptoms (symptom grade > 0)
   |			 5 = percent of subjects (%) within each cycle with severe symptoms (symptom grade >= 3)
   | Purpose   : Label frequency bars with sample size (n) or percent (%) shown on the y-axis (either )
   | Default   : 0 = no bar labels
   |
   | Name      : colors
   | Type      : 1 = Blue and red color shading, 2 = qualitative color shades, 3 = black and white 
   | Purpose   : Specify the coloring of sypmtom grades within frequency bars
   | Default   : 1 =  Blue and red color shading
   |
   | Name      : x_label
   | Type      : Character string (unquoted)
   | Purpose   : Label for the x axis of the plot
   | Default   : "Randomized Treatment Assignment" will be added if there are 2 or more arms
   |			 "Overall" will be added if no arm variable is specified 
   |
   | Name      : summary_only
   | Type      : 1 = Only display the summary measures in plots, 0 = show longitudinal time points
   | Purpose   : Surpress the individual time points from plotting. Setting summary_only=1 will 
   |			 countermand plot_limit. If summary_only=1, consider resizing the width of the figures
   |			 to a width 6 or above or adjust footnote_size to accommodate the footnotes.
   | Default   : 0 = show longitudinal time points 
   |
   | Name      : footnote_size
   | Type      : Numeric
   | Purpose   : Specify the footnote font size in points
   | Default   : 9
   |    
   | Name      : output_dir
   | Type      : Valid directory to the output folder of choice
   | Purpose   : This is the directory location where JPEG figures will be output
   | Default   : No output is created
   |
   | Name      : dpi
   | Type      : Interger
   | Purpose   : DPI (dots per inch): increase/decrease image resolution for use with output_dir 
   | Default   : 300
   |
   | Name      : zero_display
   | Type      : 1 = Display grade 0 frequency boxes within plots, 0 = do not display
   | Purpose   : Displays grade 0 counts
   | Default   : 1
   |
   | Name      : PROCTCAE_table
   | Type      : 1 = Create PRO-CTCAE variable/label reference table, 0 = do not create table
   | Purpose   : Creates a SAS dataset named 'PROCTCAE_table' listing all PRO-CTCAE variable names
   |			 and respective short lables 
   | Default   : 0 = do not create table
   |
   | Name      : debug
   | Type      : 1 = Print notes and macro values and logic for debugging, 0 = no debugging
   | Purpose   : Used for debugging unexpected results
   | Default   : 0 = no debugging
   |
   *------------------------------------------------------------------------------------------*
   | ADDITIONAL NOTES
   |
   *------------------------------------------------------------------------------------------*
   | EXAMPLES
   |
   *------------------------------------------------------------------------------------------*
   |
   | This program is free software; you can redistribute it and/or
   | modify it under the terms of the GNU General Public License as
   | published by the Free Software Foundation; either version 3 of
   | the License, or (at your option) any later version.
   |
   | This program is distributed in the hope that it will be useful,
   | but WITHOUT ANY WARRANTY; without even the implied warranty of
   | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   | General Public License for more details.
   *------------------------------------------------------------------------------------------*/

%macro PROCTCAE_toxFigures(dsn, id_var, arm_var, cycle_var, cycle_limit, cycle_fmt, baseline_val, summary_only, plot_limit, 
						   height, width, label, x_label, footnote_size, output_dir, dpi, debug, zero_display, proctcae_table, colors);
	
	
	%if %length(&dsn.)=0 and 
			%length(&id_var.)=0 and 
			%length(&arm_var.)=0 and 
			%length(&cycle_var.)=0 and
			%length(&cycle_limit.)=0 and 
			%length(&cycle_fmt.)=0 and 
			%length(&baseline_val.)=0 and 
			%length(&summary_only.)=0 and
			%length(&plot_limit.)=0 and 
			%length(&height.)=0 and 
			%length(&width.)=0 and 
			%length(&label.)=0 and
			%length(&x_label.)=0 and 
			%length(&footnote_size.)=0 and 
			%length(&output_dir.)=0 and 
			%length(&dpi.)=0 and
			%length(&debug.)=0 and 
			%length(&zero_display.)=0 and 
			%length(&colors.)=0 and 
			%length(&proctcae_table.)=0  %then %do;
		%let print_proctcae_quit = 1;
	%end;
		%else %do;
			%let print_proctcae_quit = 0;
		%end;
	
	/* ---------------------------------------------------------------------------------------------------- */	
	/* --- Allowance for debugging --- */
	/* ---------------------------------------------------------------------------------------------------- */	
	%let user_notes = %sysfunc(getoption(notes));
	%let user_mprint = %sysfunc(getoption(mprint));
	%let user_symbolgen = %sysfunc(getoption(symbolgen));
	%let user_mlogic = %sysfunc(getoption(mlogic));
	%let user_mlogicnest = %sysfunc(getoption(mlogicnest));
	%if %length(&dsn.)=0 %then %do;
		%let debug=0;
	%end;
	%if &debug.=1 %then %do;
		options notes mprint symbolgen mlogic mlogicnest;
	%end;
		%else %do;
			options nonotes nomprint nosymbolgen nomlogic nomlogicnest;
		%end;
 
	/* ---------------------------------------------------------------------------------------------------- */	
	/* --- Reference data sets --- */
	/* ---------------------------------------------------------------------------------------------------- */	
	data ____proctcae_vars;
		 length fmt_name $9 name $16 short_label $50;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_1A_SCL' ;short_label='Dry Mouth Severity' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_2A_SCL' ;short_label='Difficulty Swallowing Severity' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_3A_SCL' ;short_label='Mouth or Throat Sores Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_3B_SCL' ;short_label='Mouth or Throat Sores Interference' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_4A_SCL' ;short_label='Skin Cracking at Corners of Mouth Severity' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_5A_IND' ;short_label='Voice Changes Presence' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_6A_SCL' ;short_label='Hoarse Voice Severity' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_7A_SCL' ;short_label='Problems Tasting Severity' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_8A_SCL' ;short_label='Decreased Appetite Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_8B_SCL' ;short_label='Decreased Appetite Interference' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_9A_SCL' ;short_label='Nausea Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_9B_SCL' ;short_label='Nausea Severity' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_10A_SCL' ;short_label='Vomiting Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_10B_SCL' ;short_label='Vomiting Severity' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_11A_SCL' ;short_label='Heartburn Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_11B_SCL' ;short_label='Heartburn Severity' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_12A_IND' ;short_label='Increased Flatulence Presence' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_13A_SCL' ;short_label='Bloating of Abdomen Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_13B_SCL' ;short_label='Bloating of Abdomen Severity' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_14A_SCL' ;short_label='Hiccups Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_14B_SCL' ;short_label='Hiccups Severity' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_15A_SCL' ;short_label='Constipation Severity' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_16A_SCL' ;short_label='Diarrhea Frequency' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_17A_SCL' ;short_label='Pain in Abdomen Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_17B_SCL' ;short_label='Pain in Abdomen Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_17C_SCL' ;short_label='Pain in Abdomen Interference' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_18A_SCL' ;short_label='Loss of Bowel Control Frequency' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_18B_SCL' ;short_label='Loss of Bowel Control Interference' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_19A_SCL' ;short_label='Shortness of Breath Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_19B_SCL' ;short_label='Shortness of Breath Interference' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_20A_SCL' ;short_label='Cough Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_20B_SCL' ;short_label='Cough Interference' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_21A_SCL' ;short_label='Wheezing Severity' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_22A_SCL' ;short_label='Arm or Leg Swelling Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_22B_SCL' ;short_label='Arm or Leg Swelling Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_22C_SCL' ;short_label='Arm or Leg Swelling Interference' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_23A_SCL' ;short_label='Pounding/Racing Heartbeat Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_23B_SCL' ;short_label='Pounding/Racing Heartbeat Severity' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_24A_IND' ;short_label='Rash Presence' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_25A_SCL' ;short_label='Dry Skin Severity' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_26A_SCL' ;short_label='Acne/Pimples Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_27A_SCL' ;short_label='Hair Loss Amount' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_28A_SCL' ;short_label='Itchy Skin Severity' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_29A_IND' ;short_label='Hives Presence' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_30A_SCL' ;short_label='Hand-Foot Syndrome Severity' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_31A_IND' ;short_label='Nail Loss Presence' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_32A_IND' ;short_label='Nail Ridges/Bumps Presence' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_33A_IND' ;short_label='Nail Color Change Presence' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_34A_IND' ;short_label='Sunlight Skin Sensitivity Presence' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_35A_IND' ;short_label='Bed Sores Presence' ; output;
		 fmt_name='sev_6_fmt' ;name='PROCTCAE_36A_SCL' ;short_label='Radiation Burns Severity' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_37A_IND' ;short_label='Darkening of Skin Presence' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_38A_IND' ;short_label='Stretch Marks Presence' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_39A_SCL' ;short_label='Numbness/Tingling in Hands/Feet Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_39B_SCL' ;short_label='Numbness/Tingling in Hands/Feet Interference' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_40A_SCL' ;short_label='Dizziness Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_40B_SCL' ;short_label='Dizziness Interference' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_41A_SCL' ;short_label='Blurry Vision Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_41B_SCL' ;short_label='Blurry Vision Intererence' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_42A_IND' ;short_label='Flashing Lights in Eyes Presence' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_43A_IND' ;short_label='Eye Floaters Presence' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_44A_SCL' ;short_label='Watery Eyes Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_44B_SCL' ;short_label='Watery Eyes Interference' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_45A_SCL' ;short_label='Ringing in Ears Severity' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_46A_SCL' ;short_label='Concentration Problems Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_46B_SCL' ;short_label='Concentration Problems Interference' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_47A_SCL' ;short_label='Memory Problems Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_47B_SCL' ;short_label='Memory Problems Interference' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_48A_SCL' ;short_label='Pain Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_48B_SCL' ;short_label='Pain Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_48C_SCL' ;short_label='Pain Interference' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_49A_SCL' ;short_label='Headache Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_49B_SCL' ;short_label='Headache Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_49C_SCL' ;short_label='Headache Interference' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_50A_SCL' ;short_label='Aching Muscles Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_50B_SCL' ;short_label='Aching Muscles Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_50C_SCL' ;short_label='Aching Muscles Interference' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_51A_SCL' ;short_label='Aching Joints Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_51B_SCL' ;short_label='Aching Joints Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_51C_SCL' ;short_label='Aching Joints Interference' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_52A_SCL' ;short_label='Insomnia Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_52B_SCL' ;short_label='Insomnia Interference' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_53A_SCL' ;short_label='Fatigue Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_53B_SCL' ;short_label='Fatigue Interference' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_54A_SCL' ;short_label='Anxiety Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_54B_SCL' ;short_label='Anxiety Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_54C_SCL' ;short_label='Anxiety Interference' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_55A_SCL' ;short_label='Nothing Could Cheer You Up Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_55B_SCL' ;short_label='Nothing Could Cheer You Up Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_55C_SCL' ;short_label='Nothing Could Cheer You Up Interference' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_56A_SCL' ;short_label='Sad/Unhappy Feelings Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_56B_SCL' ;short_label='Sad/Unhappy Feelings Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_56C_SCL' ;short_label='Sad/Unhappy Feelings Interference' ; output;
		 fmt_name='yn_3_fmt' ;name='PROCTCAE_57A_IND' ;short_label='Irregular Periods Presence' ; output;
		 fmt_name='yn_3_fmt' ;name='PROCTCAE_58A_IND' ;short_label='Missed Periods Presence' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_59A_SCL' ;short_label='Unusual Vaginal Discharge Interference' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_60A_SCL' ;short_label='Vaginal Dryness Severity' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_61A_SCL' ;short_label='Pain/Burning with Urination Severity' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_62A_SCL' ;short_label='Urinary Urgency Frequency' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_62B_SCL' ;short_label='Urinary Urgency Interference' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_63A_SCL' ;short_label='Frequent Urination Frequency' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_63B_SCL' ;short_label='Frequent Urination Interference' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_64A_IND' ;short_label='Urine Color Change Presence' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_65A_SCL' ;short_label='Loss of Urine Control Frequency' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_65B_SCL' ;short_label='Loss of Urine Control Interference' ; output;
		 fmt_name='sev_7_fmt' ;name='PROCTCAE_66A_SCL' ;short_label='Erection Difficulty Severity' ; output;
		 fmt_name='frq_7_fmt' ;name='PROCTCAE_67A_SCL' ;short_label='Ejaculation Problems Frequency' ; output;
		 fmt_name='sev_7_fmt' ;name='PROCTCAE_68A_SCL' ;short_label='Decreased Sexual Interest Severity' ; output;
		 fmt_name='yn_4_fmt' ;name='PROCTCAE_69A_IND' ;short_label='Delayed Orgasm Presence' ; output;
		 fmt_name='yn_4_fmt' ;name='PROCTCAE_70A_IND' ;short_label='Unable to Orgasm Presence' ; output;
		 fmt_name='sev_7_fmt' ;name='PROCTCAE_71A_SCL' ;short_label='Pain During Vaginal Sex Severity' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_72A_SCL' ;short_label='Breast Enlargement/Tenderness Severity' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_73A_IND' ;short_label='Bruising Presence' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_74A_SCL' ;short_label='Chills Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_74B_SCL' ;short_label='Chills Severity' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_75A_SCL' ;short_label='Excessive Sweating Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_75B_SCL' ;short_label='Excessive Sweating Severity' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_76A_IND' ;short_label='Sweating Decrease Presence' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_77A_SCL' ;short_label='Hot Flashes Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_77B_SCL' ;short_label='Hot Flashes Severity' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_78A_SCL' ;short_label='Nosebleeds Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_78B_SCL' ;short_label='Nosebleeds Severity' ; output;
		 fmt_name='yn_3_fmt' ;name='PROCTCAE_79A_IND' ;short_label='Injection Site Reaction Presence' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_80A_SCL' ;short_label='Body Odor Severity' ; output;
	 run;
						   
	/* ---------------------------------------------------------------------------------------------------- */		
	/* --- Provide the user with the PROCTCAE_table reference dataset --- */
	/* ---------------------------------------------------------------------------------------------------- */	
	%if &print_proctcae_quit.=1  %then %do;
		data PROCTCAE_table;
			set ____proctcae_vars (drop=fmt_name);
			putlog @1 name= @26 short_label=;
		run;
    	%goto exit;
    %end;
    
 	/* ---------------------------------------------------------------------------------------------------- */	
	/* --- Error checks (1 of 2) --- */
	/* ---------------------------------------------------------------------------------------------------- */	
	%if %length(&dsn.)=0 %then %do;
		data _null_;
			put "ER" "ROR: No dataset was provided.";
		run;
    	%goto exit;
    %end;
    %if %sysfunc(exist(&dsn.))=0 %then %do;
		data _null_;
			put "ER" "ROR: No data live here -> &dsn..";
		run;
		%goto exit;
	%end;
	%if %length(&id_var.)=0 %then %do;
		data _null_;
			put "ER" "ROR: No subject ID variable provided.";
		run;
    	%goto exit;
    %end;
    %if %length(&cycle_var.)=0 %then %do;
		data _null_;
			put "ER" "ROR: No cycle/time variable provided.";
		run;
    	%goto exit;
    %end;
    %if %length(&baseline_val.)=0 %then %do;
		data _null_;
			put "ER" "ROR: No baseline time provided.";
		run;
    	%goto exit;
    %end;
    proc contents data=&dsn. out=____conts noprint;
	run; 
	proc sql noprint;
		select "'"||strip(upcase(name))||"'"
		into : dsn_pro_vars separated by " "
		from ____conts;
	quit;
	%let no_pro_vars=;
	data _null_;
		set ____proctcae_vars;
		if name in (&dsn_pro_vars.) then do;
			call symput("no_pro_vars", 1);
			stop;
		end;
	run;
	%if %length(&no_pro_vars.)=0 %then %do;
		data _null_;
			put "ER" "ROR: No PRO-CTCAE variables found in &dsn. fitting this macro's required format.";
		run;
    	%goto exit;
    %end;

	/* ---------------------------------------------------------------------------------------------------- */	
	/* --- Defaults --- */
	/* ---------------------------------------------------------------------------------------------------- */	
	%if &summary_only. = 1 and %length(&width.) = 0 %then %do;
		%let width = 5.8;
	%end;
	%if %length(&width.) = 0 %then %do;
		%let width = 10;
	%end;
	%if %length(&height.) = 0 %then %do;
		%let height = 6.4;
	%end;
	%if %length(&label.) = 0 %then %do;
		%let label = 0;
	%end;
	%if %length(&dpi.) = 0 %then %do;
		%let dpi = 300;
	%end;
	%if %length(&footnote_size.) = 0 %then %do;
		%let footnote_size = 9;
	%end;
	%if %length(&zero_display.) = 0 %then %do;
		%let zero_display = 1;
	%end;
	%if %length(&proctcae_table.) = 0 %then %do;
		%let proctcae_table = 0;
	%end;
	%if %length(&colors.) = 0 %then %do;
		%let colors = 1;
	%end;
	%let text_size = 8;
	%let arm_count = 1;
	%if %length(&arm_var.) ^= 0 %then %do;
		proc sql noprint;
			select count(distinct(&arm_var.))
			into : arm_count
			from &dsn.;
		quit;
		%if &arm_count. = 3 %then %do;
			%let text_size = 7;
		%end;
			%else %if &arm_count. > 3 %then %do;
				%let text_size = 6;
			%end;
	%end;
		%else %do;
			%let arm_var = __ovrlarm__;
		%end;
		
	%if %length(&x_label.) = 0 %then %do;
		%if &arm_var. = __ovrlarm__ %then %do;
			%let x_label =Overall;
		%end;
			%else %do;
				%let x_label =Randomized Treatment Assignment;
			%end;
	%end;
		
	/* ----------------------------------------------------- */
	/* --- Formats ---- */
	/* ----------------------------------------------------- */
	proc format;
		value $ fsi_fmt
			"sev" = "Severity"
			"int" = "Interference"
			"freq"= "Frequency"
			"comp"= "Composite";	
		value $ fsi_fmt
			"sev" = "Severity"
			"int" = "Interference"
			"freq"= "Frequency"
			"comp"= "Composite";	
	run;
	%if %length(&cycle_fmt.) = 0 %then %do;
		%let cycle_fmt = _cyclefmt0_;
		proc freq data=&dsn. noprint;
			table &cycle_var. / out=____cycle_frq0;
		run;
		data ____cycle_frq1;
			set ____cycle_frq0;
			length fmt_to_txt $40;
			fmt_to_txt = &cycle_var.||" = '"||propcase(strip("&cycle_var."))||" "||strip(&cycle_var.)||"'";
		run;
		proc sql noprint;
			select fmt_to_txt
			into : fmt_to_txt separated by " "
			from ____cycle_frq1;
		quit;
		proc format;
			value _cyclefmt0_
				&fmt_to_txt.;
		run;
	%end;
	proc format library = work out=____cycle_fmt_data noprint;
		select &cycle_fmt.;
	run;
	data ____cycle_fmt;
		length label $32;
		set ____cycle_fmt_data end = eof;
		fmtname = "_cyclefmt_";
		length = 12;
		default = 12;
		output;
		if eof then do;
			start = 11111;
			end = 11111;
			label = "Maximum*";
			output;
			
			start = 22222;
			end = 22222;
			label = "Adjusted**";
			output;
		end;
	run;
	proc format library=work cntlin=____cycle_fmt;
	run;
	data ____&dsn.;
		set &dsn.;
		/* limit cycles */
		%if %length(&cycle_limit.)^=0 %then %do;
			if &cycle_var. <= &cycle_limit.;
		%end;
		/* Overall stats */
		%if &arm_var. = __ovrlarm__ %then %do;
			&arm_var. = "Overall";
		%end;
			%else %do;
				if &arm_var. ^="";
			%end;
		format &cycle_var. _cyclefmt_.;
	run;
	%if &proctcae_table.=1 %then %do;
		data PROCTCAE_table;
			set ____proctcae_vars (drop=fmt_name);
		run;
	%end;
	%if &colors.=1 %then %do;
	 	data ____attrs1;
			length fillcolor $10 id $5 linecolor $10 value $18;
			fillcolor='WHITE' ;id='level' ;linecolor='CXBDBDBD' ;value='NONE' ; output;
			fillcolor='WHITE' ;id='level' ;linecolor='CXBDBDBD' ;value='NEVER' ; output;
			fillcolor='WHITE' ;id='level' ;linecolor='CXBDBDBD' ;value='NOT AT ALL' ; output;
			fillcolor='VLIGB' ;id='level' ;linecolor='VLIGB' ;value='MILD' ; output;
			fillcolor='BIGB' ;id='level' ;linecolor='BIGB' ;value='MODERATE' ; output;
			fillcolor='VIGB' ;id='level' ;linecolor='VIGB' ;value='SEVERE' ; output;
			fillcolor='DEGB' ;id='level' ;linecolor='DEGB' ;value='VERY SEVERE' ; output;
			fillcolor='VLIGB' ;id='level' ;linecolor='VLIGB' ;value='RARELY' ; output;
			fillcolor='BIGB' ;id='level' ;linecolor='BIGB' ;value='OCCASIONALLY' ; output;
			fillcolor='VIGB' ;id='level' ;linecolor='VIGB' ;value='FREQUENTLY' ; output;
			fillcolor='DEGB' ;id='level' ;linecolor='DEGB' ;value='ALMOST CONSTANTLY' ; output;
			fillcolor='VLIGB' ;id='level' ;linecolor='VLIGB' ;value='A LITTLE BIT' ; output;
			fillcolor='BIGB' ;id='level' ;linecolor='BIGB' ;value='SOMEWHAT' ; output;
			fillcolor='VIGB' ;id='level' ;linecolor='VIGB' ;value='QUITE A BIT' ; output;
			fillcolor='DEGB' ;id='level' ;linecolor='DEGB' ;value='VERY MUCH' ; output;
			fillcolor='WHITE' ;id='level' ;linecolor='CXBDBDBD' ;value='0' ; output;
			fillcolor='PAPK' ;id='level' ;linecolor='PAPK' ;value='1' ; output;
			fillcolor='STPK' ;id='level' ;linecolor='STPK' ;value='2' ; output;
			fillcolor='DEPK' ;id='level' ;linecolor='DEPK' ;value='3' ; output;
		run;
	%end;
		%else %if &colors.=2 %then %do;
			data ____attrs1;
				length fillcolor $10 id $5 linecolor $10 value $18;
				fillcolor='WHITE' ;id='level' ;linecolor='CXBDBDBD' ;value='NONE' ; output;
				fillcolor='WHITE' ;id='level' ;linecolor='CXBDBDBD' ;value='NEVER' ; output;
				fillcolor='WHITE' ;id='level' ;linecolor='CXBDBDBD' ;value='NOT AT ALL' ; output;
				fillcolor='CX0571B0' ;id='level' ;linecolor='CX0571B0' ;value='MILD' ; output;
				fillcolor='CX92C5DE' ;id='level' ;linecolor='CX92C5DE' ;value='MODERATE' ; output;
				fillcolor='CXF4A582' ;id='level' ;linecolor='CXF4A582' ;value='SEVERE' ; output;
				fillcolor='CXCA0020' ;id='level' ;linecolor='CXCA0020' ;value='VERY SEVERE' ; output;
				fillcolor='CX0571B0' ;id='level' ;linecolor='CX0571B0' ;value='RARELY' ; output;
				fillcolor='CX92C5DE' ;id='level' ;linecolor='CX92C5DE' ;value='OCCASIONALLY' ; output;
				fillcolor='CXF4A582' ;id='level' ;linecolor='CXF4A582' ;value='FREQUENTLY' ; output;
				fillcolor='CXCA0020' ;id='level' ;linecolor='CXCA0020' ;value='ALMOST CONSTANTLY' ; output;
				fillcolor='CX0571B0' ;id='level' ;linecolor='CX0571B0' ;value='A LITTLE BIT' ; output;
				fillcolor='CX92C5DE' ;id='level' ;linecolor='CX92C5DE' ;value='SOMEWHAT' ; output;
				fillcolor='CXF4A582' ;id='level' ;linecolor='CXF4A582' ;value='QUITE A BIT' ; output;
				fillcolor='CXCA0020' ;id='level' ;linecolor='CXCA0020' ;value='VERY MUCH' ; output;
				fillcolor='WHITE' ;id='level' ;linecolor='CXBDBDBD' ;value='0' ; output;
				fillcolor='CXA6DBA0' ;id='level' ;linecolor='CXA6DBA0' ;value='1' ; output;
				fillcolor='CXC2A5CF' ;id='level' ;linecolor='CXC2A5CF' ;value='2' ; output;
				fillcolor='CX7B3294' ;id='level' ;linecolor='CX7B3294' ;value='3' ; output;
			run;
		%end;
		%else %if &colors.=3 %then %do;
			data ____attrs1;
				length fillcolor $10 id $5 linecolor $10 value $18;
				fillcolor='WHITE' ;id='level' ;linecolor='CXBDBDBD' ;value='NONE' ; output;
				fillcolor='WHITE' ;id='level' ;linecolor='CXBDBDBD' ;value='NEVER' ; output;
				fillcolor='WHITE' ;id='level' ;linecolor='CXBDBDBD' ;value='NOT AT ALL' ; output;
				fillcolor='CXE6E6E6' ;id='level' ;linecolor='CXBDBDBD' ;value='MILD' ; output;
				fillcolor='CXBDBDBD' ;id='level' ;linecolor='CXBDBDBD' ;value='MODERATE' ; output;
				fillcolor='CX636363' ;id='level' ;linecolor='CX636363' ;value='SEVERE' ; output;
				fillcolor='CX000000' ;id='level' ;linecolor='CX000000' ;value='VERY SEVERE' ; output;
				fillcolor='CXE6E6E6' ;id='level' ;linecolor='CXBDBDBD' ;value='RARELY' ; output;
				fillcolor='CXBDBDBD' ;id='level' ;linecolor='CXBDBDBD' ;value='OCCASIONALLY' ; output;
				fillcolor='CX636363' ;id='level' ;linecolor='CX636363' ;value='FREQUENTLY' ; output;
				fillcolor='CX000000' ;id='level' ;linecolor='CX000000' ;value='ALMOST CONSTANTLY' ; output;
				fillcolor='CXE6E6E6' ;id='level' ;linecolor='CXBDBDBD' ;value='A LITTLE BIT' ; output;
				fillcolor='CXBDBDBD' ;id='level' ;linecolor='CXBDBDBD' ;value='SOMEWHAT' ; output;
				fillcolor='CX636363' ;id='level' ;linecolor='CX636363' ;value='QUITE A BIT' ; output;
				fillcolor='CX000000' ;id='level' ;linecolor='CX000000' ;value='VERY MUCH' ; output;
				fillcolor='WHITE' ;id='level' ;linecolor='CXBDBDBD' ;value='0' ; output;
				fillcolor='CXBDBDBD' ;id='level' ;linecolor='CXBDBDBD' ;value='1' ; output;
				fillcolor='CX636363' ;id='level' ;linecolor='CX636363' ;value='2' ; output;
				fillcolor='CX000000' ;id='level' ;linecolor='CX000000' ;value='3' ; output;
			run;
		%end;
	%if &zero_display.=0 %then %do;
	 	data ____attrs1;
			set ____attrs1;
			if value in ("NONE", "NEVER", "NOT AT ALL", "0") then linecolor = "WHITE";
		run;
 	%end;
	
	/* ----------------------------------------------------- */
	/* --- Iterate through vars within dsn ---- */
	/* ----------------------------------------------------- */
	data ____var_conts0;
		set ____proctcae_vars;
		qnum = input(compress(name,, "kd"), best8.);
	run;
	data ____proctcae_comp_vars;
		set ____proctcae_vars;
		num = input(compress(name,,"kd"), best8.);
	run;
	data ____proctcae_comp_vars(keep=name comp_label);
		set ____proctcae_comp_vars (rename=(name=org_name));
		by num;
		if first.num;
		call scan(short_label, -1, pos, length);
		comp_label=substr(short_label, 1, pos-2);
		name = "PROCTCAE_"||strip(num)||"_comp";
	run;
	data ____comp_var_conts0;
		set ____proctcae_comp_vars;
		qnum = input(compress(name,, "kd"), best8.);
		format comp_label $32.;
	run;
	data ____conts_together0;
		length name $26;
		set ____var_conts0 (rename=(short_label = label)) ____comp_var_conts0 (rename=(comp_label = label));
		name = lowcase(name);
		if index(name, "_comp") then item = "comp";
			else item = compress(scan(name, 2, "_"), "", "d"); 
		drop fmt_name;
	run;
	proc sql noprint;
		select "'"||lowcase(strip(name))||"'"
		into : proctcae_var_names separated by  " "
		from ____proctcae_vars
		/* -- Remove yes/no proctcae questions -- */
		where index(lowcase(name), "_ind")=0;
		
		select "'"||lowcase(strip(name))||"'"
		into : proctcae_comp_names separated by  " "
		from ____proctcae_comp_vars;
	quit;
	data ____&dsn._conts;
		set ____conts (where=(lowcase(name) in (&proctcae_var_names. &proctcae_comp_names.)) keep=name);
		name = lowcase(name);
	run;
	proc sort data=____&dsn._conts;
		by name;
	run;
	proc sort data=____conts_together0;
		by name;
	run;
	data ____&dsn._vars0;
		merge ____&dsn._conts (in=a) ____conts_together0 (in=b);
		by name;
		if a and b;
		drop label;
	run;
	proc sort data=____&dsn._vars0;
		by qnum;
	run;
	proc transpose data=____&dsn._vars0 out=____&dsn._var_tn(drop=_name_ _label_);
		by qnum;
		id item;
		var name;
	run;
	proc sort data=____comp_var_conts0;
		by qnum;
	run;
	data ____&dsn._var_ref;
		merge ____&dsn._var_tn (in=one) ____comp_var_conts0 (in=two rename=(comp_label = label) drop=name);
		by qnum;
		if one and two;
		rank + 1;
	run;
	
	/* ---------------------------------------------------------------------------------------------------- */	
	/* --- Error checks (2 of 2) --- */
	/* ---------------------------------------------------------------------------------------------------- */
	proc sort data=&dsn.(keep=&id_var. &cycle_var.) out=_null_ nodupkey dupout=____dup_check0;
		by &id_var. &cycle_var.;
	run;
	data ____dup_check;
		merge &dsn.(in=a keep=&id_var. &cycle_var.) ____dup_check0 (in=b);
		by &id_var. &cycle_var.;
		if b;
	run;
	%let dup_count=0;
	proc sql noprint;
		select count(unique(&id_var.))
		into : dup_count
		from ____dup_check;
	quit;
	%if &dup_count.>0 %then %do;
		data _null_;
			put "ER" "ROR: There were %sysfunc(strip(&dup_count.)) individuals within %sysfunc(strip(&dsn.)) with duplicate observations at a single cycle.";
			put "ER" "ROR: Duplicate observations will lead to invalid interpretations of results presented here.";
			put "ER" "ROR: Duplicate observations shown below:";
		run;
		data _null_;
			set ____dup_check;
			observation = "ID:"||strip(&id_var.)||" CYCLE:"||strip(&cycle_var.);
			put observation=;
		run;
		%goto exit;
	%end;
	%let indi_vars=;
	%let comp_vars=;
	%let bad_obs=;
	proc sql noprint;
		select name
		into : indi_vars separated by " "
		from ____&dsn._conts
		where index(upcase(name), "_COMP")=0;
		select name
		into : comp_vars separated by " "
		from ____&dsn._conts
		where index(upcase(name), "_COMP")>0;
	quit;
	/* --- Individual items --- */
	%if %length(&indi_vars.) > 0 %then %do;
		data _null_;
			set ____&dsn.;
			array indi_vars(*) &indi_vars.;
			do i=1 to dim(indi_vars);
				if indi_vars(i) ^ in (.,0,1,2,3,4) then do;
					call symput("bad_obs", 1);
					_obs_number_ = _n_;
					put "ER" "ROR: Numerical PRO-CTCAE item responses should be integers between 0 and 4.";
					put "ER" "ROR: See observation number and unexpected PRO-CTCAE response below.";
					put _obs_number_=;
					put indi_vars(i)=;
				end;
			end;
		run;
	%end;
	/* --- Composite items --- */
	%if %length(&comp_vars.) > 0 %then %do;
		data _null_;
			set ____&dsn.;
			array comp_vars(*) &comp_vars.;
			do i=1 to dim(comp_vars);
				if comp_vars(i) ^ in (.,0,1,2,3) then do;
					call symput("bad_obs", 1);
					_obs_number_ = _n_;
					put "ER" "ROR: Numerical PRO-CTCAE Composite responses should be integers between 0 and 3.";
					put "ER" "ROR: See observation number and unexpected Composite response below.";
					put _obs_number_=;
					put comp_vars(i)=;
				end;
			end;
		run;
	%end;
	%if &bad_obs.=1 %then %do;
		%goto exit;
	%end;
	
	/* ----------------------------------------------------- */
	/* --- Start iteration ---- */
	/* ----------------------------------------------------- */
	proc sql noprint;
		select max(rank)
		into : max_rank
		from ____&dsn._var_ref;
	quit;
	%let i = 1;
	%do %while(&i. <= &max_rank.);
		%let var_a =;
		%let var_b =;
		%let var_c =;
		%let var_comp =;
		%let comp_label =;
		data _null_;
			set ____&dsn._var_ref (where=(rank=&i.));
			call symput("var_a", a);
			call symput("var_b", b);
			call symput("var_c", c);
			call symput("var_comp", comp);
			call symput("comp_label", label);
		run;
				
		/* ----------------------------------------------------- */
		/* --- ITEM A ---- */
		/* ----------------------------------------------------- */
		%let apply_a_fmt=;
		%let apply_c_fmt=;
		%let apply_b_fmt=;
		%if %length(&var_a.)>0 %then %do;
			proc sort data=____&dsn.;
				by &arm_var. &cycle_var.;
			run;
			proc freq data=____&dsn. noprint;
				table &var_a. / out=____a_freq_out0(where=(&var_a. ^= .));
				by &arm_var. &cycle_var.;
			run;
			proc sql noprint;
				create table ____a_freq_out as
				select *, sum(count) as tot, "a" as name
				from ____a_freq_out0
				group by &arm_var., &cycle_var.;
			quit;			
			proc sort data=____&dsn.;
				by &id_var. &cycle_var.;
			run;
			data ____dsn_out_coding0;
				set ____&dsn.;
				by &id_var.;
				if first.&id_var. then base_score = .;
				retain base_score;
				__temp_score = .;
				if &cycle_var. = &baseline_val. then base_score = &var_a.;
					else __temp_score = &var_a.;
			run;
			proc sql noprint;
				create table ____dsn_out_coding1 as 
				select *,
					max(__temp_score) as max_post_bl,		
					(case
						when &cycle_var. ^= &baseline_val. and base_score = . then .
						when &cycle_var. ^= &baseline_val. and base_score >= max(&var_a.) then  0
						when &cycle_var. ^= &baseline_val. and base_score < max(&var_a.) then max(&var_a.)
						end) as bl_adjusted
				from ____dsn_out_coding0
				group by &id_var., &arm_var.
				order by &id_var., &cycle_var.;
			quit;			
			data ____dsn_out_coding2;
				set ____dsn_out_coding1;
				by &id_var.;
				if last.&id_var.;
				if max_post_bl =. then bl_adjusted =.; /***************** astrxx */
				drop &cycle_var. &var_a.;
			run;
			proc sort data=____dsn_out_coding2;
				by &arm_var.;
			run;
			proc freq data=____dsn_out_coding2 noprint;
				table max_post_bl / out=____max_out(where=(max_post_bl ^= .));
				by &arm_var.;
			run;
			proc sql noprint;
				create table ____max_out_count (rename=(max_post_bl=score)) as
				select *, sum(count) as tot, "a" as name, 11111 as &cycle_var.
				from ____max_out
				group by &arm_var.;
			quit;
			proc freq data=____dsn_out_coding2 noprint;
				table bl_adjusted / out=____bl_adj_out(where=(bl_adjusted ^= .));
				by &arm_var.;
			run;
			proc sql noprint;
				create table ____bl_adj_out_count (rename=(bl_adjusted=score)) as
				select *, sum(count) as tot, "a" as name, 22222 as &cycle_var.
				from ____bl_adj_out
				group by &arm_var.;
			quit;
			proc sql noprint;
				select substr(fmt_name, 1, 3)
				into : apply_a_fmt separated by " "
				from ____proctcae_vars
				where upcase(name) = upcase("&var_a.");
			quit;
			data ____a_fin_counts;
				set ____a_freq_out (rename=(&var_a. = score))
					____max_out_count 
					____bl_adj_out_count;
				length score2 $18;
				%if "&apply_a_fmt." = "frq" %then %do;
					if score = 0 then score2 = "NEVER";
						else if score = 1 then score2 = "RARELY";
						else if score = 2 then score2 = "OCCASIONALLY";
						else if score = 3 then score2 = "FREQUENTLY";
						else if score = 4 then score2 = "ALMOST CONSTANTLY";
				%end;
					%else %if "&apply_a_fmt." = "sev" %then %do;
						if score = 0 then score2 = "NONE";
							else if score = 1 then score2 = "MILD";
							else if score = 2 then score2 = "MODERATE";
							else if score = 3 then score2 = "SEVERE";
							else if score = 4 then score2 = "VERY SEVERE";
					%end;
					%else %if "&apply_a_fmt." = "int" %then %do;
						if score = 0 then score2 = "NOT AT ALL";
							else if score = 1 then score2 = "A LITTLE BIT";
							else if score = 2 then score2 = "SOMEWHAT";
							else if score = 3 then score2 = "QUITE A BIT";
							else if score = 4 then score2 = "VERY MUCH";
					%end;
				format &cycle_var. _cyclefmt_.;
				drop score;
				rename score2 = score;
			run;
		%end;
		
		/* ----------------------------------------------------- */
		/* --- ITEM B ---- */
		/* ----------------------------------------------------- */
		%if %length(&var_b.)>0 %then %do;
			proc sort data=____&dsn.;
				by &arm_var. &cycle_var.;
			run;
			proc freq data=____&dsn. noprint;
				table &var_b. / out=____b_freq_out0(where=(&var_b. ^= .));
				by &arm_var. &cycle_var.;
			run;
			proc sql noprint;
				create table ____b_freq_out as
				select *, sum(count) as tot, "b" as name
				from ____b_freq_out0
				group by &arm_var., &cycle_var.;
			quit;
			proc sort data=____&dsn.;
				by &id_var. &cycle_var.;
			run;				
			data ____dsn_out_coding0;
				set ____&dsn.;
				by &id_var.;
				if first.&id_var. then base_score = .;
				retain base_score;
				__temp_score = .;
				if &cycle_var. = &baseline_val. then base_score = &var_b.;
					else __temp_score = &var_b.;
			run;
			proc sql noprint;
				create table ____dsn_out_coding1 as 
				select *,
					max(__temp_score) as max_post_bl,
			
					(case
						when &cycle_var. ^= &baseline_val. and base_score = . then .
						when &cycle_var. ^= &baseline_val. and base_score >= max(&var_b.) then  0
						when &cycle_var. ^= &baseline_val. and base_score < max(&var_b.) then max(&var_b.)
						end) as bl_adjusted
						
				from ____dsn_out_coding0
				group by &id_var., &arm_var.
				order by &id_var., &cycle_var.;
			quit;
			data ____dsn_out_coding2;
				set ____dsn_out_coding1;
				by &id_var.;
				if last.&id_var.;
				if max_post_bl =. then bl_adjusted =.; /***************** astrxx */
				drop &cycle_var. &var_b.;
			run;
			proc sort data=____dsn_out_coding2;
				by &arm_var.;
			run;
			proc freq data=____dsn_out_coding2 noprint;
				table max_post_bl / out=____max_out(where=(max_post_bl ^= .));
				by &arm_var.;
			run;
			proc sql noprint;
				create table ____max_out_count (rename=(max_post_bl=score)) as
				select *, sum(count) as tot, "b" as name, 11111 as &cycle_var.
				from ____max_out
				group by &arm_var.;
			quit;
			proc freq data=____dsn_out_coding2 noprint;
				table bl_adjusted / out=____bl_adj_out(where=(bl_adjusted ^= .));
				by &arm_var.;
			run;
			proc sql noprint;
				create table ____bl_adj_out_count (rename=(bl_adjusted=score)) as
				select *, sum(count) as tot, "b" as name, 22222 as &cycle_var.
				from ____bl_adj_out
				group by &arm_var.;
			quit;
			proc sql noprint;
				select substr(fmt_name, 1, 3)
				into : apply_b_fmt separated by " "
				from ____proctcae_vars
				where upcase(name) = upcase("&var_b.");
			quit;
			data ____b_fin_counts;
				set ____b_freq_out(rename=(&var_b. = score)) 
					____max_out_count 
					____bl_adj_out_count;
				length score2 $18;				
				%if "&apply_b_fmt." = "frq" %then %do;
					if score = 0 then score2 = "NEVER";
						else if score = 1 then score2 = "RARELY";
						else if score = 2 then score2 = "OCCASIONALLY";
						else if score = 3 then score2 = "FREQUENTLY";
						else if score = 4 then score2 = "ALMOST CONSTANTLY";
				%end;
					%else %if "&apply_b_fmt." = "sev" %then %do;
						if score = 0 then score2 = "NONE";
							else if score = 1 then score2 = "MILD";
							else if score = 2 then score2 = "MODERATE";
							else if score = 3 then score2 = "SEVERE";
							else if score = 4 then score2 = "VERY SEVERE";
					%end;
					%else %if "&apply_b_fmt." = "int" %then %do;
						if score = 0 then score2 = "NOT AT ALL";
							else if score = 1 then score2 = "A LITTLE BIT";
							else if score = 2 then score2 = "SOMEWHAT";
							else if score = 3 then score2 = "QUITE A BIT";
							else if score = 4 then score2 = "VERY MUCH";
					%end;
				format &cycle_var. _cyclefmt_.;
				drop score;
				rename score2 = score;
			run;
		%end;

		/* ----------------------------------------------------- */
		/* --- ITEM C ---- */
		/* ----------------------------------------------------- */	
		%if %length(&var_c.)>0 %then %do;
			proc sort data=____&dsn.;
				by &arm_var. &cycle_var.;
			run;
			proc freq data=____&dsn. noprint;
				table &var_c. / out=____c_freq_out0(where=(&var_c. ^= .));
				by &arm_var. &cycle_var.;
			run;
			proc sql noprint;
				create table ____c_freq_out as
				select *, sum(count) as tot, "c" as name
				from ____c_freq_out0
				group by &arm_var., &cycle_var.;
			quit;
			proc sort data=____&dsn.;
				by &id_var. &cycle_var.;
			run;	
			data ____dsn_out_coding0;
				set ____&dsn.;
				by &id_var.;
				if first.&id_var. then base_score = .;
				retain base_score;
				__temp_score = .;
				if &cycle_var. = &baseline_val. then base_score = &var_c.;
					else __temp_score = &var_c.;
			run;
			proc sql noprint;
				create table ____dsn_out_coding1 as 
				select *,
					max(__temp_score) as max_post_bl,			
					(case
						when &cycle_var. ^= &baseline_val. and base_score = . then .
						when &cycle_var. ^= &baseline_val. and base_score >= max(&var_c.) then  0
						when &cycle_var. ^= &baseline_val. and base_score < max(&var_c.) then max(&var_c.)
						end) as bl_adjusted						
				from ____dsn_out_coding0
				group by &id_var., &arm_var.
				order by &id_var., &cycle_var.;
			quit;
			data ____dsn_out_coding2;
				set ____dsn_out_coding1;
				by &id_var.;
				if last.&id_var.;
				if max_post_bl =. then bl_adjusted =.; /***************** astrxx */
				drop &cycle_var. &var_c.;
			run;
			proc sort data=____dsn_out_coding2;
				by &arm_var.;
			run;
			proc freq data=____dsn_out_coding2 noprint;
				table max_post_bl / out=____max_out(where=(max_post_bl ^= .));
				by &arm_var.;
			run;
			proc sql noprint;
				create table ____max_out_count (rename=(max_post_bl=score)) as
				select *, sum(count) as tot, "c" as name, 11111 as &cycle_var.
				from ____max_out
				group by &arm_var.;
			quit;
			proc freq data=____dsn_out_coding2 noprint;
				table bl_adjusted / out=____bl_adj_out(where=(bl_adjusted ^= .));
				by &arm_var.;
			run;
			proc sql noprint;
				create table ____bl_adj_out_count (rename=(bl_adjusted=score)) as
				select *, sum(count) as tot, "c" as name, 22222 as &cycle_var.
				from ____bl_adj_out
				group by &arm_var.;
			quit;			
			proc sql noprint;
				select substr(fmt_name, 1, 3)
				into : apply_c_fmt separated by " "
				from ____proctcae_vars
				where upcase(name) = upcase("&var_c.");
			quit;						
			data ____c_fin_counts;
				set ____c_freq_out(rename=(&var_c. = score)) 
					____max_out_count 
					____bl_adj_out_count;
				length score2 $18;	
				%if "&apply_c_fmt." = "frq" %then %do;
					if score = 0 then score2 = "NEVER";
						else if score = 1 then score2 = "RARELY";
						else if score = 2 then score2 = "OCCASIONALLY";
						else if score = 3 then score2 = "FREQUENTLY";
						else if score = 4 then score2 = "ALMOST CONSTANTLY";
				%end;
					%else %if "&apply_c_fmt." = "sev" %then %do;
						if score = 0 then score2 = "NONE";
							else if score = 1 then score2 = "MILD";
							else if score = 2 then score2 = "MODERATE";
							else if score = 3 then score2 = "SEVERE";
							else if score = 4 then score2 = "VERY SEVERE";
					%end;
					%else %if "&apply_c_fmt." = "int" %then %do;
						if score = 0 then score2 = "NOT AT ALL";
							else if score = 1 then score2 = "A LITTLE BIT";
							else if score = 2 then score2 = "SOMEWHAT";
							else if score = 3 then score2 = "QUITE A BIT";
							else if score = 4 then score2 = "VERY MUCH";
					%end;
				format &cycle_var. _cyclefmt_.;
				drop score;
				rename score2 = score;
			run;
		%end;
		
		/* ----------------------------------------------------- */
		/* --- ITEM COMPOSITE ---- */
		/* ----------------------------------------------------- */
		%if %length(&var_comp.)>0 %then %do;
			proc sort data=____&dsn.;
				by &arm_var. &cycle_var.;
			run;			
			proc freq data=____&dsn. noprint;
				table &var_comp. / out=____comp_freq_out0(where=(&var_comp. ^= .));
				by &arm_var. &cycle_var.;
			run;
			proc sql noprint;
				create table ____comp_freq_out as
				select *, sum(count) as tot, "comp" as name
				from ____comp_freq_out0
				group by &arm_var., &cycle_var.;
			quit;
			proc sort data=____&dsn.;
				by &id_var. &cycle_var.;
			run;					
			data ____dsn_out_coding0;
				set ____&dsn.;
				by &id_var.;
				if first.&id_var. then base_score = .;
				retain base_score;
				__temp_score = .;
				if &cycle_var. = &baseline_val. then base_score = &var_comp.;
					else __temp_score = &var_comp.;
			run;
			proc sql noprint;
				create table ____dsn_out_coding1 as 
				select *,
					max(__temp_score) as max_post_bl,			
					(case
						when &cycle_var. ^= &baseline_val. and base_score = . then .
						when &cycle_var. ^= &baseline_val. and base_score >= max(&var_comp.) then  0
						when &cycle_var. ^= &baseline_val. and base_score < max(&var_comp.) then max(&var_comp.)
						end) as bl_adjusted		
						
				from ____dsn_out_coding0
				group by &id_var., &arm_var.
				order by &id_var., &cycle_var.;
			quit;
			data ____dsn_out_coding2;
				set ____dsn_out_coding1;
				by &id_var.;
				if last.&id_var.;
				if max_post_bl =. then bl_adjusted =.; /***************** astrxx */
				drop &cycle_var. &var_comp.;
			run;
			proc sort data=____dsn_out_coding2;
				by &arm_var.;
			run;	
			proc freq data=____dsn_out_coding2 noprint;
				table max_post_bl / out=____max_out(where=(max_post_bl ^= .));
				by &arm_var.;
			run;
			proc sql noprint;
				create table ____max_out_count (rename=(max_post_bl=score)) as
				select *, sum(count) as tot, "comp" as name, 11111 as &cycle_var.
				from ____max_out
				group by &arm_var.;
			quit;
			proc freq data=____dsn_out_coding2 noprint;
				table bl_adjusted / out=____bl_adj_out(where=(bl_adjusted ^= .));
				by &arm_var.;
			run;
			proc sql noprint;
				create table ____bl_adj_out_count (rename=(bl_adjusted=score)) as
				select *, sum(count) as tot, "comp" as name, 22222 as &cycle_var.
				from ____bl_adj_out
				group by &arm_var.;
			quit;
			data ____comp_fin_counts;
				set ____comp_freq_out(rename=(&var_comp. = score)) 
					____max_out_count 
					____bl_adj_out_count;
				score2 = put(score, 18.);
				drop score;
				rename score2 = score;
				format &cycle_var. _cyclefmt_.;
			run;
		%end;

		/* ----------------------------------------------------- */
		/* --- Together ---- */
		/* ----------------------------------------------------- */	
		data ____together;
			length name $4;
			set 
				%if %sysfunc(exist(____a_fin_counts))^=0 %then %do;
					____a_fin_counts
				%end;
				%if %sysfunc(exist(____b_fin_counts))^=0 %then %do;
					____b_fin_counts
				%end;
				%if %sysfunc(exist(____c_fin_counts))^=0 %then %do;
					____c_fin_counts
				%end;
				%if %sysfunc(exist(____comp_fin_counts))^=0 %then %do;
					____comp_fin_counts
				%end;;
		run;
		data ____together1;
			length name $18;
			set ____together;
			if name = "a" then sort_flag = 1;
				else if name = "b" then sort_flag = 2;
				else if name = "c" then sort_flag = 3;
				else if name = "comp" then sort_flag = 4;
			if strip(score) in ("RARELY" "MILD" "A LITTLE BIT" "1") then sort_flag2 = 1;
				else if strip(score) in ("OCCASIONALLY" "MODERATE" "SOMEWHAT" "2") then sort_flag2 = 2;
				else if strip(score) in ("FREQUENTLY" "SEVERE" "QUITE A BIT" "3") then sort_flag2 = 3;
				else if strip(score) in ("ALMOST CONSTANTLY" "VERY SEVERE" "VERY MUCH") then sort_flag2 = 4;				
				%if "&apply_a_fmt." = "frq" %then %do;
					if name = "a" then name = "Frequency";
				%end;
					%else %if "&apply_a_fmt." = "sev" %then %do;
						if name = "a" then name = "Severity";
					%end;
					%else %if "&apply_a_fmt." = "int" %then %do;
						if name = "a" then name = "Interference";
					%end;
				%if "&apply_b_fmt." = "frq" %then %do;
					if name = "b" then name = "Frequency";
				%end;
					%else %if "&apply_b_fmt." = "sev" %then %do;
						if name = "b" then name = "Severity";
					%end;
					%else %if "&apply_b_fmt." = "int" %then %do;
						if name = "b" then name = "Interference";
					%end;
				%if "&apply_c_fmt." = "frq" %then %do;
					if name = "c" then name = "Frequency";
				%end;
					%else %if "&apply_c_fmt." = "sev" %then %do;
						if name = "c" then name = "Severity";
					%end;
					%else %if "&apply_c_fmt." = "int" %then %do;
						if name = "c" then name = "Interference";
					%end;
		run;
		
		/* - Skip figure output processing if there is no figure data - */
		%let _open_ = %sysfunc(open(____together1));
		%let _nobs_ = %sysfunc(attrn(&_open_., nobs));
		%let _close_ = %sysfunc(close(&_open_.));
		%if &_nobs_. = 0 %then %goto skip;
		/*  ----------------------------------------------------------- */
		
		proc sql noprint;
			/* Counts for  all */
			%if &label.=0 or &label.=1  %then %do;
				create table ____together2 as
				select *, sum(count) as n_val, round(sum(percent),1) as perc_val
				from ____together1
				group by name, &arm_var., &cycle_var.
				order by &cycle_var., name, &arm_var.;
			%end;
				/* Counts for present */
				%else %if &label.=2 or &label.=4 %then %do;
					create table ____together2 as
					select *, 
						round(sum(case when 
							strip(score) ^ in ("0", "NEVER", "NOT AT ALL", "NONE") 
							then percent else . end),1) as perc_val,
						sum(case when 
							strip(score) ^ in ("0", "NEVER", "NOT AT ALL", "NONE") 
							then count else . end) as n_val
					from ____together1
					group by name, &arm_var., &cycle_var.
					order by &cycle_var., name, &arm_var.;
				%end;
				
				/* Counts for severe */
				%else %if &label.=3 or &label.=5 %then %do;
					create table ____together2 as
					select *, 
						round(sum(case when 
							strip(score) in ("FREQUENTLY", "ALMOST CONSTANTLY",
									  		"SEVERE", "VERY SEVERE", 
									  		"QUITE A BIT", "VERY MUCH", 
											"3") 
							then percent else . end),1) as perc_val,
						sum(case when 
							strip(score) in ("FREQUENTLY", "ALMOST CONSTANTLY",
									  		"SEVERE", "VERY SEVERE", 
									  		"QUITE A BIT", "VERY MUCH", 
											"3") 
							then count else . end) as n_val
					from ____together1
					group by name, &arm_var., &cycle_var.
					order by &cycle_var., name, &arm_var.;
				%end;
		quit;
		data ____together2;
			set ____together2;
			if perc_val = . then perc_val = 0;
			if n_val = . then n_val = 0;
		run;
		data ____together3;
			set ____together2;
			length lab $12 lab_perc $12;
			by &cycle_var. name &arm_var.;
			retain lab lab_perc;
			if first.name then do;
				lab="";
				lab_perc="";
			end;
			if first.name and &cycle_var. = &baseline_val. then lab="N=";
			%if &arm_count. < 3 %then %do; 
				if first.&arm_var. then do;
					lab = catx("  ",lab, n_val);
					lab_perc = catx("  ",lab_perc, perc_val);
				end;
			%end;
				%else %if &arm_count. >= 3 %then %do; 
					if first.&arm_var. then do;
						lab = catx(" ",lab, n_val);
						lab_perc = catx(" ",lab_perc, perc_val);
					end;
				%end;
			lab = tranwrd(lab, "N= ", "N=");
			lab_perc = tranwrd(lab_perc, "%= ", "%=");
		run;
		data ____together4;
			set ____together3;
			by &cycle_var. name &arm_var.;
			if last.name=0 then lab = "";
		run;
		proc sort data=____together4;
			by &cycle_var. name descending &arm_var. descending lab;
		run;
		
		data ____together5_0;
			set ____together4;
			by &cycle_var. name;
			retain n_val_lab perc_val_lab;
			if first.name then do;
				n_val_lab = lab;
				perc_val_lab = lab_perc;
			end;
		run;
		proc sort data=____together5_0;
			by &cycle_var. &arm_var. sort_flag descending sort_flag2 ;
		run;
		proc sql noprint;
			create table ____together5_1 as
			select *, count(score) as level_count
			from ____together5_0
			group by &cycle_var., &arm_var., sort_flag;
		quit;
		data ____together5_2;
			set ____together5_1;
			by &cycle_var. &arm_var. sort_flag descending sort_flag2 ;
			retain rank;
			if first.sort_flag then rank=0;
			rank +1;
		run;
		proc sort data=____together5_2;
			by &cycle_var. &arm_var. sort_flag descending sort_flag2 ;
		run;
		data ____together5;
			set ____together5_2;
			/* Centering the count labeling for bars in plots */
			if level_count=2 then do;
				if rank ^= 2 then do;
					n_val=.;
					perc_val=.;
				end;
			end;
				else if level_count=3 then do;
					if rank ^= 2 then do;
						n_val=.;
						perc_val=.;
					end;
				end;
				else if level_count=4 then do;
					if rank ^= 2 then do;
						n_val=.;
						perc_val=.;
					end;
				end;
				else if level_count=5 then do;
					if rank ^= 3 then  do;
						n_val=.;
						perc_val=.;
					end;
				end;
		run;
		proc sort data=____together5;
			by &cycle_var. sort_flag descending sort_flag2 &arm_var. ;
		run;
		data _null_;
			call symput("imgfile_name", tranwrd(tranwrd(strip("&comp_label.")," ","_"),"/","_"));
		run;
		
		/*  ------------------------------------------------ */
		/* - Print title to results but not to ods output - */
		data ____title;
			title = "&comp_label.";
		run;
		proc report data=____title nowindows noheader nocenter style(column)=[fontsize=4];
		run;
		/* ------------------------------------------------- */ 
		
		%if &label.=1 or &label.=2 or &label.=3 %then %do;
			%let foot_type = number;
			%let foot_symbol = (n);
		%end;
			%if &label.=4 or &label.=5 %then %do;
				%let foot_type = percent;
			%let foot_symbol = (%);
			%end;
		
		%if &label.=2 or &label.=4 %then %do;
			%let foot_grade = 1;
		%end;
			%if &label.=3 or &label.=5 %then %do;
				%let foot_grade = 3;
			%end;
		
		%if %length(&output_dir.)=0 %then %do;
			ods graphics / reset width = &width.in height=&height.in border=off;
		%end;
			%else %if %length(&output_dir.)^=0 %then %do;
				ods graphics / reset imagename="&imgfile_name." imagefmt=JPEG width = &width.in height=&height.in  border=off;
			%end;
		proc sql noprint;
			select dequote(fillcolor)
			into : col1
			from ____attrs1
			where value = "MILD";
			select dequote(fillcolor)
			into : col2
			from ____attrs1
			where value = "MODERATE";
			select dequote(fillcolor)
			into : col3
			from ____attrs1
			where value = "SEVERE";
			select dequote(fillcolor)
			into : col4
			from ____attrs1
			where value = "VERY SEVERE";
			select dequote(fillcolor)
			into : comp1
			from ____attrs1
			where value = "1";
			select dequote(fillcolor)
			into : comp2
			from ____attrs1
			where value = "2";
			select dequote(fillcolor)
			into : comp3
			from ____attrs1
			where value = "3";
		quit;
		%if %lowcase("&label.") = "n" or %lowcase("&label.") = "percent" %then %do;
			%if "&apply_a_fmt." = "frq" or "&apply_b_fmt." = "frq" or "&apply_c_fmt." = "frq" %then %do;
				footnote1 height=&footnote_size.pt justify=LEFT bold "Frequency: " c=&col1. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black " Rarely / "
				  c=&col2. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black " Occasionally / "
				  c=&col3. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black " Frequently / "
				  c=&col4. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black " Almost constantly "
				  ;
			%end;
			%if "&apply_a_fmt." = "sev" or "&apply_b_fmt." = "sev" or "&apply_c_fmt." = "sev" %then %do;
				footnote2 height=&footnote_size.pt justify=LEFT bold "Severity: " c=&col1. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black " Mild / "
				  c=&col2. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black " Moderate / "
				  c=&col3. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black " Severe / "
				  c=&col4. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black " Very severe "
				  ;
			%end;
			%if "&apply_a_fmt." = "int" or "&apply_b_fmt." = "int" or "&apply_c_fmt." = "int" %then %do;
				footnote3 height=&footnote_size.pt justify=LEFT bold "Interference: " c=&col1. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black " A little bit / "
				  c=&col2. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black " Somewhat / "
				  c=&col3. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black " Quite a bit / "
				  c=&col4. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black " Very much "
				  ;
			%end;
		%end;
		
			%else %if %lowcase("&label.") ^= "n" and %lowcase("&label.") ^= "percent" %then %do;
				%if "&apply_a_fmt." = "frq" or "&apply_b_fmt." = "frq" or "&apply_c_fmt." = "frq" %then %do;
					footnote1 height=&footnote_size.pt justify=LEFT bold "Frequency: "
					  %if &zero_display.=1 %then %do;
					  		c=gray font="sans-serif" " (*ESC*){unicode '25A1'x} " c=black "Never /"
					  %end;
					  c=&col1. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black "Rarely /"
					  c=&col2. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black "Occasionally /"
					  c=&col3. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black "Frequently /"
					  c=&col4. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black "Almost constantly"
					  ;
				%end;
				%if "&apply_a_fmt." = "sev" or "&apply_b_fmt." = "sev" or "&apply_c_fmt." = "sev" %then %do;
					footnote2 height=&footnote_size.pt justify=LEFT bold "Severity: "
					  %if &zero_display.=1 %then %do;
					  		c=gray font="sans-serif" " (*ESC*){unicode '25A1'x} " c=black "None /"
					  %end;
					  c=&col1. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black "Mild /"
					  c=&col2. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black "Moderate /"
					  c=&col3. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black "Severe /"
					  c=&col4. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black "Very severe"
					  ;
				%end;
				%if "&apply_a_fmt." = "int" or "&apply_b_fmt." = "int" or "&apply_c_fmt." = "int" %then %do;
					footnote3 height=&footnote_size.pt justify=LEFT bold "Interference: "
					  %if &zero_display.=1 %then %do;
					  		c=gray font="sans-serif" " (*ESC*){unicode '25A1'x} " c=black "Not at all / "
					  %end;
					  c=&col1. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black "A little bit /"
					  c=&col2. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black "Somewhat /"
					  c=&col3. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black "Quite a bit /"
					  c=&col4. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black "Very much"
					  ;
				%end;
			%end;
		
		%if %length(&var_comp.)>0 %then %do;
			footnote4 height=&footnote_size.pt justify=LEFT bold "Composite Grade: " 
			  %if &zero_display.=1 %then %do;
			  		c=gray font="sans-serif" " (*ESC*){unicode '25A1'x} " c=black "0 /"
			  %end;
			  c=&comp1. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black "1 /"
			  c=&comp2. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black "2 /"
			  c=&comp3. font="sans-serif" " (*ESC*){unicode '25A0'x} " c=black "3"
			  ;
		%end;
		%if &label.=1 %then %do;
			footnote5 height=&footnote_size.pt justify=LEFT "Column labels (n) show the number of subjects with an observed symptom score or grade.";
		%end;
			%else %if &label.=2 or &label.=3 or &label.=4 or &label.=5 %then %do;
				footnote5 height=&footnote_size.pt justify=LEFT "Column labels &foot_symbol. show the &foot_type. of subjects with symptom score or grade &foot_grade. or greater.";
			%end;
		footnote6 height=&footnote_size.pt justify=LEFT "*Maximum score or grade reported post-baseline per patient.";
		footnote7 height=&footnote_size.pt justify=LEFT "**Maximum score or grade reported post-baseline per patient when including only scores that were worse than the patient's baseline score.";
		%if %length(&output_dir.)^=0 %then %do;
			ods listing gpath="&output_dir." image_dpi=&dpi.;
		%end;
		title "&comp_label.";
		title;
		ods graphics / width = &width.in height = &height.in;
		proc sgpanel data=____together5 dattrmap=____attrs1 noautolegend;
			where score ^= "0"
				%if %length(&plot_limit.)^=0 %then %do;
					and (&cycle_var. <= &plot_limit. or &cycle_var. >= 11111)
				%end;
				%if &summary_only.=1 %then %do;
					and &cycle_var. in (11111, 22222)
				%end;;
			label &arm_var. =
				%if &arm_var. = __ovrlarm__ %then %do;
					"&x_label."
				%end;
					%else %do;
						"&x_label."
					%end;;
			format &cycle_var. _cyclefmt_. name $fsi_fmt.;
			panelby &cycle_var. name / layout = lattice novarname onepanel sort=data 
				%if &arm_var. = __ovrlarm__ %then %do;
					colheaderpos=both
				%end;;
			vbar &arm_var. / response=percent group = score attrid=level grouporder=data;
			%if &label.=1 or &label.=2 or &label.=3 %then %do;
				colaxistable n_val / stat=mean label="n:" position=top nomissingchar nomissingclass droponmissing
					classdisplay=cluster separator pad=(top=4) labelpos=left;
			%end;
				%if &label.=4 or &label.=5 %then %do;
					colaxistable perc_val / stat=mean label="%:" position=top nomissingchar nomissingclass droponmissing
						classdisplay=cluster separator pad=(top=4) labelpos=left;
				%end;
			rowaxis min=0 max=100 values=(0 20 40 60 80 100) fitpolicy=thin;
			colaxis fitpolicy=rotatethin /*offsetmax=.6 offsetmin=.6*/
				%if &arm_var. = __ovrlarm__ %then %do;
					display=none
				%end;;
		run;
		%if %length(&output_dir.)^=0 %then %do;
			ods listing close;
			ods listing;
		%end;
		footnote1;
		footnote2;
		footnote3;
		footnote4;
		footnote5;
		footnote6;
		footnote7;
		%skip:
		proc datasets noprint;
			delete
 				%if %sysfunc(exist(____a_fin_counts))^=0 %then %do;
					____a_fin_counts
				%end;
				%if %sysfunc(exist(____b_fin_counts))^=0 %then %do;
					____b_fin_counts
				%end;
				%if %sysfunc(exist(____c_fin_counts))^=0 %then %do;
					____c_fin_counts
				%end;
				%if %sysfunc(exist(____comp_fin_counts))^=0 %then %do;
					____comp_fin_counts
				%end;
 				_____________;
		quit;
	%let i = %eval(&i.+1);
	%end;
	
	/* ------------------------------ */
	/* --- Clean up ----------------- */
	/* ------------------------------ */
	ods graphics / reset;
	%exit:
	proc datasets noprint;
		delete _sgsrt2_ ____:;
	quit;
	options &user_notes. &user_mprint. &user_symbolgen. &user_mlogic. &user_mlogicnest.;
%mend;


