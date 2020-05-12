
 /*-------------------------------------------------------------------------------------------*
   | MACRO NAME	 : PROCTCAE_toxTables
   | SHORT DESC  : Recodes PRO-CTCAE survey responses and calculates composite scores
   |				
   |				
   *------------------------------------------------------------------------------------------*
   | AUTHORS  	 : Blake T Langlais, Amylou C Dueck
   *------------------------------------------------------------------------------------------*
   | MODIFIED BY : 
   *------------------------------------------------------------------------------------------*
   | PURPOSE	 : This macro takes in a SAS data set with numeric PRO-CTCAE survey variables 
   |			   then outputs an Excel data file with toxcity tables for individual and 
   |			   composite survey items. Rates of symptomatic adverse events >0 and >= 3
   |			   are compared between/among arms using chi sqaure or Fisher's exact tests.
   |			   Risk differences between two arms are also available in lieu of statistical tests.
   |				   
   |			   PRO-CTCAE variable names MUST conform to a pre-specified naming structure. Whereby PRO-CTCAE 
   |			   variable names are made up of FOUR components: 1)'PROCTCAE', 2) number [1,2,3, ..., i, ..., 80], 
   |               3) 'A', 'B', or 'C' component of the i-th PRO-CTCAE field, 4) and 'SCL' (if severity,
   |               interference, or frequency) or 'IND' (if yes/no variable). Each component
   |               must be delimitated by an underscore (_) 
   |                  EX1: Question 1 of PRO-CTCAE should be: PROCTCAE_1A_SCL
   |                  EX2: Question 48 of PRO-CTCAE should be: PROCTCAE_48A_SCL, PROCTCAE_48B_SCL, PROCTCAE_48C_SCL
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
	%PROCTCAE_toxTables(dsn = ,
						id_var = ,
						cycle_var = ,
						baseline_val = ,
						output_dir = ,
						output_filename = );
						
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
   | Type      : Valid variable name
   | Purpose   : Field name of variable differentiating one longitudinal/repeated PRO-CTCAE
   |             suvey from another, within an individual ID
   |    
   | Name      : baseline_val
   | Type      : Numerical value for baseline cycle/time
   | Purpose   : This is the value indicating an individual's baseline time point (e.g. cycle 1, time 0, visit 1) 
   |
   | Name      : output_dir
   | Type      : Valid directory to the output folder of choice
   | Purpose   : This is the directory location where Excel files will be output
   |    
   | Name      : output_filename
   | Type      : Valid filename for the output Excel file. Do not include file extension (e.g., .xlsx, .xls)
   | Purpose   : This will be the name of the output file found within the specified output directory   
   *------------------------------------------------------------------------------------------*
   | OPTIONAL PARAMETERS
   |
   | Name      : arm_var
   | Type      : Valid variable name (must be a character variable)
   | Purpose   : Field name of arm variable differentiating treatment groups
   | Default   : Overall frequencies will be reported (if no arm/grouping variable is provided)
   |
   | Name      : test
   | Type      : c = chi square, f = fisher's exact
   | Purpose   : Specifies the statistical test to apply comparing rates among arms
   | Default   : Chi square test
   |
   | Name      : fmt_pvalues
   | Type      : 1 = format p values, 0 = report p values to four digits
   | Purpose   : Formats p values or allows user to have p values reported to four digits
   | Default   : 1 = format p values
   |
   | Name      : riskdiff
   | Type      : 1 = Calculates risk differences between two arms 
   | Purpose   : Calls for the risk diffrence calculations. Valid if there are only two arms in the
   |			 dsn specified. This option will countermand options called with the 'test' parameter
   | Default   : NA
   |
   | Name      : type
   | Type      : max_post_bl = Use subjects' maximum score post baseline visit
   |		     bl_adjusted = Use subjects' baseline adjusted score over the study period
   | Default   : bl_adjusted
   | 
   | Name      : cycle_limit
   | Type      : Numeric
   | Purpose   : Limit the data to be analyzed up to and including a given cycle number
   | Default   : All available cycle time points are used
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



%macro PROCTCAE_toxTables(dsn, id_var, arm_var, cycle_var, baseline_val, cycle_limit, type, test, fmt_pvalues, riskdiff, output_dir, output_filename);
	
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
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_13A_SCL' ;short_label='Bloating of the Abdomen Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_13B_SCL' ;short_label='Bloating of the Abdomen Severity' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_14A_SCL' ;short_label='Hiccups Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_14B_SCL' ;short_label='Hiccups Severity' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_15A_SCL' ;short_label='Constipation Severity' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_16A_SCL' ;short_label='Diarrhea Frequency' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_17A_SCL' ;short_label='Abdominal Pain Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_17B_SCL' ;short_label='Abdominal Pain Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_17C_SCL' ;short_label='Abdominal Pain Interference' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_18A_SCL' ;short_label='Bowel Incontinence Frequency' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_18B_SCL' ;short_label='Bowel Incontinence Interference' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_19A_SCL' ;short_label='Shortness of Breath Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_19B_SCL' ;short_label='Shortness of Breath Interference' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_20A_SCL' ;short_label='Cough Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_20B_SCL' ;short_label='Cough Interference' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_21A_SCL' ;short_label='Wheezing Severity' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_22A_SCL' ;short_label='Arm or Leg Swelling Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_22B_SCL' ;short_label='Arm or Leg Swelling Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_22C_SCL' ;short_label='Arm or Leg Swelling Interference' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_23A_SCL' ;short_label='Racing Heartbeat Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_23B_SCL' ;short_label='Racing Heartbeat Severity' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_24A_IND' ;short_label='Rash Presence' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_25A_SCL' ;short_label='Dry Skin Severity' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_26A_SCL' ;short_label='Acne Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_27A_SCL' ;short_label='Hair Loss Amount' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_28A_SCL' ;short_label='Itchy Skin Severity' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_29A_IND' ;short_label='Hives Presence' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_30A_SCL' ;short_label='Hand-Foot Syndrome Severity' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_31A_IND' ;short_label='Nail Loss Presence' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_32A_IND' ;short_label='Nail Ridges Presence' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_33A_IND' ;short_label='Nail Color Change Presence' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_34A_IND' ;short_label='Sunlight Skin Sensitivity Presence' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_35A_IND' ;short_label='Bed Sores Presence' ; output;
		 fmt_name='sev_6_fmt' ;name='PROCTCAE_36A_SCL' ;short_label='Skin Burns from Radiation Severity' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_37A_IND' ;short_label='Darkening of Skin Presence' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_38A_IND' ;short_label='Stretch Marks Presence' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_39A_SCL' ;short_label='Numbness or Tingling in Hands or Feet Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_39B_SCL' ;short_label='Numbness or Tingling in Hands or Feet Interference' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_40A_SCL' ;short_label='Dizziness Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_40B_SCL' ;short_label='Dizziness Interference' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_41A_SCL' ;short_label='Blurry Vision Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_41B_SCL' ;short_label='Blurry Vision Intererence' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_42A_IND' ;short_label='Flashing Lights in Eyes Presence' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_43A_IND' ;short_label='Floaters in Eyes Presence' ; output;
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
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_56A_SCL' ;short_label='Sadness Frequency' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_56B_SCL' ;short_label='Sadness Severity' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_56C_SCL' ;short_label='Sadness Interference' ; output;
		 fmt_name='yn_3_fmt' ;name='PROCTCAE_57A_IND' ;short_label='Irregular Periods Presence' ; output;
		 fmt_name='yn_3_fmt' ;name='PROCTCAE_58A_IND' ;short_label='Missed Periods Presence' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_59A_SCL' ;short_label='Vaginal Discharge Amount' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_60A_SCL' ;short_label='Vaginal Dryness Severity' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_61A_SCL' ;short_label='Pain with Urination Severity' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_62A_SCL' ;short_label='Urine Urgency Frequency' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_62B_SCL' ;short_label='Urine Urgency Interference' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_63A_SCL' ;short_label='Frequent Urination Frequency' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_63B_SCL' ;short_label='Frequent Urination Interference' ; output;
		 fmt_name='yn_2_fmt' ;name='PROCTCAE_64A_IND' ;short_label='Urine Color Change Presence' ; output;
		 fmt_name='frq_5_fmt' ;name='PROCTCAE_65A_SCL' ;short_label='Urine Incontinence Frequency' ; output;
		 fmt_name='int_5_fmt' ;name='PROCTCAE_65B_SCL' ;short_label='Urine Incontinence Interference' ; output;
		 fmt_name='sev_7_fmt' ;name='PROCTCAE_66A_SCL' ;short_label='Erection Difficulty Severity' ; output;
		 fmt_name='frq_7_fmt' ;name='PROCTCAE_67A_SCL' ;short_label='Ejaculation Problems Frequency' ; output;
		 fmt_name='sev_7_fmt' ;name='PROCTCAE_68A_SCL' ;short_label='Decreased Sexual Interest Severity' ; output;
		 fmt_name='yn_4_fmt' ;name='PROCTCAE_69A_IND' ;short_label='Delayed Orgasm Presence' ; output;
		 fmt_name='yn_4_fmt' ;name='PROCTCAE_70A_IND' ;short_label='Unable to Orgasm Presence' ; output;
		 fmt_name='sev_7_fmt' ;name='PROCTCAE_71A_SCL' ;short_label='Pain During Vaginal Sex Severity' ; output;
		 fmt_name='sev_5_fmt' ;name='PROCTCAE_72A_SCL' ;short_label='Breast Enlargement or Tenderness Severity' ; output;
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
	/* --- Error checks --- */
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
    %if %length(&baseline_val.)=0 %then %do;
		data _null_;
			put "ER" "ROR: No baseline time provided.";
		run;
    	%goto exit;
    %end;
	%if %length(&output_dir.)=0 %then %do;
		data _null_;
			put "ER" "ROR: No output directory provided.";
		run;
    	%goto exit;
    %end;   
    %if %length(&output_filename.)=0 %then %do;
		data _null_;
			put "ER" "ROR: No output filename provided.";
		run;
    	%goto exit;
    %end;    
	%if %length(&riskdiff.)=0 %then %do;
    	%let riskdiff=0;
	%end;
	%if %length(&fmt_pvalues.)=0 %then %do;
    	%let fmt_pvalues=1;
	%end;
	%if %length(&test.)=0 %then %do;
    	%let test=c;
	%end;
/* 		%else %if %lowcase(%sysfunc(strip("&test."))) ^= ("f" "c") %then %do; */
/* 			%let test =c; */
/* 		%end; */
	
	%if %length(&type.) = 0 %then %do;
		%let type = bl_adjusted;
	%end;
		%else %do;
			%let type = %lowcase(&type.);
		%end;
		
	%if %lowcase(%sysfunc(strip("&type."))) ^= ("max_post_bl") %then %do;
		%let type = bl_adjusted;
	%end;
	
	%if &type. = bl_adjusted %then %do;
		%let adjust_label = With baseline adjustment;
	%end;
		%else %if &type. = max_post_bl %then %do;
			%let adjust_label = With max score post baseline;
		%end;

    %if %length(&arm_var.) ^= 0 %then %do;
		proc sql noprint;
			select count(distinct(&arm_var.))
			into : arm_count
			from &dsn.;
		quit;
	%end;
		%else %if %length(&arm_var.) = 0 %then %do;
			%let arm_count = 1;
			%let arm_var = __ovrlarm__;
		%end;
    
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
		name = "PROCTCAE_"||strip(num)||"_COMP";
	run;
	/* ---------------------------------------------------------------------------------------------------- */	
	/* --- Defaults / references --- */
	/* ---------------------------------------------------------------------------------------------------- */	
	proc format;
		invalue present_fmt
			0 = 0
			1,2,3,4 = 1
			other = .;
		invalue severe_fmt
			0,1,2 = 0
			3,4 = 1
			other = .;
		value pv_bold
			low - < .05 = "bold";
	run;
	proc contents data=&dsn. out=____&dsn._conts noprint;
	run;
	
	/* --------------------------------------------------------------------------------------- */
	/* --- Get baseline adjusted and unadjusted max scores for each subject accross cycles --- */
	/* --------------------------------------------------------------------------------------- */
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
	run;
	proc sort data=____&dsn.;
		by &id_var. &cycle_var.;
	run;
	data ____vars_ref;
		set ____&dsn._conts (keep=name);
		where index(upcase(name), "PROCTCAE_")>0 and index(upcase(name), "_IND")=0; /* remove y/n items */
		rank + 1;
	run;
	/* --- Ref list of IDs in dsn for use in summary measure do loop below --- */
	proc sort data=____&dsn.(keep=&id_var. &arm_var.) nodupkey out=____id_score;
		by &id_var.;
	run;
	proc sql noprint;
		select max(rank)
		into : max_rank
		from ____vars_ref;
	quit;
	%put "&max_rank.";
	%let i = 1;
	%do %while(&i. <= &max_rank.);
		data _null_;
			set ____vars_ref (where=(rank=&i.));
			call symput("var_i", strip(name));
		run;
		%put "&var_i.";
		data ____dsn_out_coding0;
			set ____&dsn. (keep=&id_var. &var_i. &cycle_var.);
			by &id_var.;
			if first.&id_var. then base_score = .;
			retain base_score;
			if &cycle_var. = &baseline_val. then base_score = &var_i.;
				else __temp_score = &var_i.;
		run;
		proc sql noprint;
			create table ____dsn_out_coding1 as 
			select *,
				max(__temp_score) as max_post_bl,
					
				(case
					when &cycle_var. ^= &baseline_val. and base_score = . then .
					when &cycle_var. ^= &baseline_val. and base_score >= max(&var_i.) then  0
					when &cycle_var. ^= &baseline_val. and base_score < max(&var_i.) then max(&var_i.)
					end) as bl_adjusted
					
			from ____dsn_out_coding0
			group by &id_var.
			order by &id_var., &cycle_var.;
		quit;
		data ____dsn_out_coding2;
			set ____dsn_out_coding1(keep = &id_var. &type.);
			by &id_var.;
			if last.&id_var.;
			rename &type. = &var_i.;
		run;
		data ____id_score;
			merge ____id_score(in=a)
				  ____dsn_out_coding2(in=b);
			by &id_var.;
			if a;
		run;
		%let i = %eval(&i.+1);
	%end;
	/* --------------------------------------------------------------------------------------- */
	/* --------------------------------------------------------------------------------------- */

	proc sql noprint;
		select  strip(name)||"_present = input("||strip(name)||", present_fmt.);",
				strip(name)||"_severe = input("||strip(name)||", severe_fmt.);",
				strip(name)||"_present",
				strip(name)||"_severe"
		into : present_fmts separated by " ",
			 : severe_fmts separated by " ",
			 : present_vars separated by " ",
			 : severe_vars separated by " "
		from ____&dsn._conts
		where index(upcase(name), "PROCTCAE_")>0 and index(upcase(name), "_IND")=0 /* remove y/n items */
		order by varnum;
	quit;
	data ____&dsn.2;
		set ____id_score;
		&present_fmts.
		&severe_fmts.
	run;
	proc sort data=____&dsn.2;
		by &arm_var.;
	run;
	ods output CrossTabFreqs=____ct 
			%if &arm_count. > 1 %then %do; ChiSq=____cs FishersExact=____fe %end;
			%if &arm_count. = 2 %then %do; RiskDiffCol2=____rd0 %end;;
	proc freq data=____&dsn.2;
		table &arm_var.*(&present_vars. &severe_vars.) / nocol nopercent chisq fisher riskdiff out=____ct_check;
	run;
	
	/* ----------------------------------------------------------------------------- */
	/* --- Get counts and presence rates for 0/1+ and (1,2)>3+ by arm --- */
	/* ----------------------------------------------------------------------------- */
	proc sql noprint;
		create table ____ct2 as
		select *, sum(frequency) as tot_nonmiss
		from ____ct
		where _type_ = "11" and not missing(&arm_var.)
		group by &arm_var., table;
	quit;
	data ____ct3;
		set ____ct2;
		combine = sum(of PROCTCAE_:);
		item = scan(table,3);
		rate = frequency||" ("||strip(round(RowPercent,1))||"%)";
		if combine = 1;
		rename tot_nonmiss = arm_n;
		keep item &arm_var. tot_nonmiss rate;
	run;
	proc sort data=____ct3;
		by item;
	run;
	
	/* ----------------------- */
	/* -- counts ------------- */
	/* ----------------------- */
	data ____ct4 (drop=org_&arm_var.);
		set ____ct3(rename=(&arm_var. = org_&arm_var.));
		&arm_var. = strip(org_&arm_var.)||"_n";
	run;
	proc transpose data=____ct4 out=____arm_item_counts0;
		by item;
		id &arm_var.;
		var arm_n;
	run;
	data ____arm_item_counts1(drop=org_item);
		set ____arm_item_counts0(rename=(item = org_item));
		by org_item;
		item = catx("_", scan(org_item,1,"_"), scan(org_item,2,"_"), scan(org_item,3,"_"));
	run;
	data ____arm_item_counts(drop=_name_);
		set ____arm_item_counts1;
		by item;
		if first.item;
	run;
	
	/* ----------------------- */
	/* -- Rates -------------- */
	/* ----------------------- */
	data ____ct5 (drop=org_&arm_var.);
		set ____ct3(rename=(item=item_2 &arm_var. = org_&arm_var.));
		&arm_var. = strip(org_&arm_var.)||"_rate";
	run;	
	proc transpose data=____ct5 out=____arm_item_rates(drop=_name_);
		by item_2;
		id &arm_var.;
		var rate;
	run;
	%if &arm_var. = __ovrlarm__ %then %do;
		data ____overall_item_rates;
			set ____arm_item_rates;
			item = catx("_", scan(item_2,1,"_"), scan(item_2,2,"_"), scan(item_2,3,"_"));
			type = scan(item_2,-1,"_");
		run;		
		proc sort data=____arm_item_counts;
			by item;
		run;
		
		data ____overall_item_rates2;
			merge ____overall_item_rates(in=a) ____arm_item_counts(in=b);
			by item;
			if a and b;
		run;		
		data ____pres;
			set ____overall_item_rates2 (rename=(overall_rate = overall_pres));
			if type ="present";
		run;
		data ____sev;
			set ____overall_item_rates2 (rename=(overall_rate = overall_sev));
			if type ="severe";
		run;
		data ____item_counts;
			set ____pres (keep=item overall_n);
		run;
		proc sort data = ____item_counts;
			by item;
		run;		
		proc sort data = ____pres;
			by item;
		run;		
		proc sort data = ____sev; 
			by item;
		run;
		data ____proctcae_labels;
			set ____proctcae_comp_vars(rename=(name=item comp_label=label) keep=name comp_label) 
				____proctcae_vars(rename=(name=item short_label=label) keep=name short_label);
		run;
		proc sort data = ____proctcae_labels;
			by item;
		run;
		data ____table_dat ____table_dat_indi ____table_dat_comp;
			merge ____item_counts(in=a) 
				  ____pres(in=b) 
				  ____sev(in=c) 
				  ____proctcae_labels (in=d);
			by item;
			if a and b and c;
			qnum = input(compress(item,,"kd"),best8.);
			output ____table_dat;
			if index(item, "_COMP")>0 then output ____table_dat_comp;
				else output ____table_dat_indi;
		run;
		proc sort data = ____table_dat;
			by qnum;
		run;
		proc sort data = ____table_dat_comp;
			by qnum;
		run;
		proc sort data = ____table_dat_indi;
			by qnum;
		run;
		
		/* ----------------------------------------------------------------------------- */
		/* --- Build the final tables for overall group (reports) --- */
		/* ----------------------------------------------------------------------------- */		
		options papersize=(22in 27in);
		ods excel file="&output_dir./&output_filename..xlsx";			
		ods excel options(sheet_name="PRO-CTCAE Individual");
		proc report data=____table_dat_indi style(report)=[frame=void rules=group];
			label 	label = "PRO-CTCAE Individual Item Analysis" 
					Overall_n = 'Overall' 
					Overall_pres = "n (%)"
					Overall_sev="n (%)" ;
			column label ("N"  overall_n) 
						 ("&adjust_label. >0" overall_pres) 
						 ("&adjust_label. >=3" overall_sev);
			define Overall_n / center;
			define overall_pres / center;
			define overall_sev / center;
			define label / style(column header)={borderrightcolor=black};
			define Overall_n / style(column header)={borderrightcolor=black};
		run;
		ods excel options(sheet_name="PRO-CTCAE Composite");
		proc report data=____table_dat_comp;
			label label = "PRO-CTCAE Composite Item Analysis"
					Overall_n = 'Overall' 
					Overall_pres = "n (%)"
					Overall_sev="n (%)" ;
			column label ("N"  overall_n) 
						 ("&adjust_label. >0" overall_pres) 
						 ("&adjust_label. >=3" overall_sev);
			define Overall_n / center;
			define overall_pres / center;
			define overall_sev / center;
			define label / style(column header)={borderrightcolor=black};
			define Overall_n / style(column header)={borderrightcolor=black};
		run;
		ods excel close;
	%end;
		%else %do;
			/* ----------------------------------------------------------------------------- */
			/* --- Attach the chi square and fisher pv's to the arm_item_rates --- */
			/* ----------------------------------------------------------------------------- */
			data ____cs1;
				set ____cs (rename=(prob = pv));
				item_2 = scan(table,3);
				test = "chisqr";
				if statistic = "Chi-Square";
				keep item_2 pv test;
			run;
			data ____fe1;
				set ____fe (rename=(nvalue1 = pv));
				item_2 = scan(table,3);
				test = "fisher";
				%if &arm_count. > 2 %then %do;
					if strip(label1) = "Pr <= P";
				%end;
					%else %do;
						if strip(label1) = "Two-sided Pr <= P";
					%end;
				format pv pvalue6.5;
				keep item_2 pv test;
			run;
			data ____cs_fe_pvs0;
				set ____cs1 ____fe1;
				item = catx("_", scan(item_2,1,"_"), scan(item_2,2,"_"), scan(item_2,3,"_"));
				type = scan(item_2,-1,"_");
			run;
			/* ------------------------ */
			/* -- Attach counts ------- */
			/* ------------------------ */
			proc sort data=____cs_fe_pvs0;
				by item;
			run;
			proc sort data=____arm_item_counts;
				by item;
			run;
			data ____cs_fe_pvs1;
				merge ____cs_fe_pvs0(in=a) ____arm_item_counts(in=b);
				by item;
				if a and b;
			run;
			/* ------------------------ */
			/* -- Attach rates -------- */
			/* ------------------------ */
			proc sort data=____cs_fe_pvs1;
				by item_2;
			run;	
			proc sort data=____arm_item_rates;
				by item_2;
			run;
			data ____cs_fe_pvs2;
				merge ____cs_fe_pvs1(in=a) ____arm_item_rates(in=b);
				by item_2;
				if b;
				/* -- Allow identical group frequencies to pass through with resulting pv of '-' in toxTable -- */
				if pv = . then do;
					do test = "chisqr", "fisher";
						pv = 9;
						type = scan(item_2, -1, "_");
						item = substr(item_2, 1, index(item_2, strip(type))-2);
						output;
					end;
				end;
					else output;
			run;
			proc sql noprint;
				select distinct(&arm_var.)
				into : arm_n separated by " "
				from ____ct4;
				
				select distinct(&arm_var.)
				into : arm_rate separated by " "
				from ____ct5;
			quit;
			data ____ct4_adj;
				set ____ct4;
				keep item &arm_var. arm_n;
			run;
			proc transpose data=____ct4_adj out=____ct4_adj_trans(drop=_name_);
				by item;
				id &arm_var.;
				var arm_n;
			run;
			proc sort data=____ct4_adj_trans;
				by item;
			run;
			proc sort data=____cs_fe_pvs2;
				by item_2;
			run;
			data ____cs_fe_pvs2;
				merge 	____cs_fe_pvs2(in=a drop=&arm_n.)
						____ct4_adj_trans (in=b rename=(item=item_2));
				by item_2;
				if a;
			run;
			proc transpose data=____cs_fe_pvs2(where=(type="present")) out=____present_pvs(where=(chisqr^=.) drop=_name_);
				by item;
				id test;
				var pv;
				copy &arm_n. &arm_rate.;
			run;
			proc transpose data=____cs_fe_pvs2(where=(type="severe")) out=____severe_pvs(where=(chisqr^=.) drop=_name_);
				by item;
				id test;
				var pv;
				copy &arm_n. &arm_rate.;
			run;

			/* ----------------- */
			/* -- item/counts -- */
			/* ----------------- */
			proc contents data=____present_pvs noprint out=____present_pvs_conts;
			run;		
			data ____item_counts;
				set ____present_pvs (keep=item &arm_n.);
			run;
		
			/* ---------------------------- */
			/* -- item/present: rate/pvs -- */
			/* ---------------------------- */
			data ____item_pres_rtpv0;
				set ____present_pvs (drop= &arm_n.);
			run;
			proc sql noprint;
				select strip(name)||"="||strip(name)||"_pres"
				into : pres_renm separated by " "
				from ____present_pvs_conts
				where substr(name, length(name)-4, 5) = "_rate";
			quit;
			data ____item_pres_rtpv;
				set ____item_pres_rtpv0 (rename = (chisqr=chisqr_pres fisher=fisher_pres &pres_renm.));
			run;
			
			/* ---------------------------- */
			/* -- item/severe: rate/pvs -- */
			/* ---------------------------- */
			data ____item_sev_rtpv0;
				set ____severe_pvs (drop= &arm_n.);
			run;
			proc sql noprint;
				select strip(name)||"="||strip(name)||"_sev"
				into : sev_renm separated by " "
				from ____present_pvs_conts
				where substr(name, length(name)-4, 5) = "_rate";
			quit;	
			data ____item_sev_rtpv;
				set ____item_sev_rtpv0 (rename = (chisqr=chisqr_sev fisher=fisher_sev &sev_renm.));
			run;
			proc sort data = ____item_counts;
				by item;
			run;
			proc sort data = ____item_pres_rtpv;
				by item;
			run;	
			proc sort data = ____item_sev_rtpv;
				by item;
			run;
			data ____proctcae_labels;
				set ____proctcae_comp_vars(rename=(name=item comp_label=label) keep=name comp_label) 
					____proctcae_vars(rename=(name=item short_label=label) keep=name short_label);
			run;
			proc sort data = ____proctcae_labels;
				by item;
			run;
			data ____table_dat ____table_dat_indi ____table_dat_comp;
				merge ____item_counts(in=a) 
					  ____item_pres_rtpv(in=b) 
					  ____item_sev_rtpv(in=c) 
					  ____proctcae_labels (in=d);
				by item;
				if a and b and c;
				qnum = input(compress(item,,"kd"),best8.);
				array pv_blank(*) fisher_sev fisher_pres chisqr_sev chisqr_pres;
				do j=1 to dim(pv_blank);
					 if pv_blank(j) = 9 then pv_blank(j) = .;
				end;
				%if &fmt_pvalues. = 1 %then %do;
					array pv_num(*) fisher_sev fisher_pres chisqr_sev chisqr_pres;
					array pv_fmt(*) $ fisher_sev0 fisher_pres0 chisqr_sev0 chisqr_pres0;
					do i=1 to dim(pv_fmt);
						if 0.001 <= pv_num(i) < 0.01 then pvaltemp=put(round(pv_num(i),.001),6.3); 			
						if 0.01	<= pv_num(i) <0.045 then pvaltemp=put(round(pv_num(i),.01),6.2);	
						if 0.045 <= pv_num(i) < 0.05 then pvaltemp=put(round(pv_num(i),.001),6.3); 			
						if pv_num(i) >= 0.05 then pvaltemp=put(round(pv_num(i),.01),6.2); 						
						if pvaltemp ^=. then pv_fmt(i) = strip(pvaltemp);
						if 0 < pv_num(i) < 0.001 then pv_fmt(i) = strip("<0.001");
						if pv_num(i)=1 then pv_fmt(i)=">0.99";
						if pv_num(i)=. then pv_fmt(i)="-";
					end;
					drop fisher_sev fisher_pres chisqr_sev chisqr_pres i pvaltemp;
					rename	fisher_sev0 = fisher_sev 
							fisher_pres0 = fisher_pres 
							chisqr_sev0 = chisqr_sev 
							chisqr_pres0 = chisqr_pres;
				%end;
				output ____table_dat;
				if index(item, "_COMP")>0 then output ____table_dat_comp;
					else output ____table_dat_indi;
			run;
			proc sort data = ____table_dat;
				by qnum;
			run;
			proc sort data = ____table_dat_comp;
				by qnum;
			run;
			proc sort data = ____table_dat_indi;
				by qnum;
			run;		
			%if &arm_count. = 2 and &riskdiff. = 1 %then %do;
			
				/* ----------------------------------------------------------------------------- */
				/* --- Get risk differences --- */
				/* ----------------------------------------------------------------------------- */
				data ____rd;
					set ____rd0;
					if row = "Difference";
					/*risk_ci = put(risk, 5.2)||" ("||put(lowercl, 5.2)||", "||put(uppercl, 5.2)||")";*/
					risk_ci = put(risk, percentn7.0)||" ("||strip(put(lowercl, percentn7.0))||", "||strip(put(uppercl, percentn7.0))||")";
					item_2 = strip(scan(Table,2,"*"));
					item = catx("_", scan(item_2,1,"_"), scan(item_2,2,"_"), scan(item_2,3,"_"));
					type = scan(item_2,-1,"_");
					drop ExactLowerCL ExactUpperCL Control row ase;
				run;
				proc sort data=____rd;
					by item;
				run;	
				data ____rd1;
					merge ____rd(in=a) ____arm_item_counts(in=b);
					by item;
					if a and b;
					drop Table Risk LowerCL UpperCL;
				run;		
				proc sort data=____rd1;
					by item_2;
				run;		
				data ____rd2;
					merge ____rd1(in=a) ____arm_item_rates(in=b);
					by item_2;
					if a and b;
				run;		
				data ____rd_sev ____rd_pres;
					set ____rd2;
					if type = "present" then output ____rd_pres;
						else if type = "severe" then output ____rd_sev;
					keep item A_n B_n A_rate B_rate risk_ci;
				run;
		
				/* ---------------------------- */
				/* -- item/present: rate/risk -- */
				/* ---------------------------- */
				proc contents data=____rd_pres noprint out=____rd_pres_conts;
				run;	
				data ____item_pres_rd0;
					set ____rd_pres (drop= &arm_n.);
				run;
				proc sql noprint;
					select strip(name)||"="||strip(name)||"_pres"
					into : rd_pres_renm separated by " "
					from ____rd_pres_conts
					where substr(name, length(name)-4, 5) = "_rate";
				quit;
				data ____item_pres_rd;
					set ____item_pres_rd0 (rename = (risk_ci = risk_ci_pres &rd_pres_renm.));
				run;
		
				/* ---------------------------- */
				/* -- item/severe: rate/pvs --- */
				/* ---------------------------- */
				data ____item_sev_rd0;
					set ____rd_sev (drop= &arm_n.);
				run;
				proc sql noprint;
					select strip(name)||"="||strip(name)||"_sev"
					into : rd_sev_renm separated by " "
					from ____rd_pres_conts
					where substr(name, length(name)-4, 5) = "_rate";
				quit;
				data ____item_sev_rd;
					set ____item_sev_rd0 (rename = (risk_ci = risk_ci_sev &rd_sev_renm.));
				run;
				proc sort data = ____item_counts;
					by item;
				run;
				proc sort data = ____item_pres_rd;
					by item;
				run;		
				proc sort data = ____item_sev_rd;
					by item;
				run;
				data ____table_dat_rd ____table_dat_indi_rd ____table_dat_comp_rd;
					merge ____item_counts(in=a) 
						  ____item_pres_rd(in=b) 
						  ____item_sev_rd(in=c) 
						  ____proctcae_labels (in=d);
					by item;
					if a and b and c;
					qnum = input(compress(item,,"kd"),best8.);
					output ____table_dat_rd;
					if index(item, "_COMP")>0 then output ____table_dat_comp_rd;
						else output ____table_dat_indi_rd;
				run;
				proc sort data = ____table_dat_rd;
					by qnum;
				run;		
				proc sort data = ____table_dat_comp_rd;
					by qnum;
				run;	
				proc sort data = ____table_dat_indi_rd;
					by qnum;
				run;
			%end;
		
			/* ----------------------------------------------------------------------------- */
			/* --- Build the final tables (reports) --- */
			/* ----------------------------------------------------------------------------- */
			proc contents data=____table_dat out=____table_dat_conts noprint;
			run;
			proc sql noprint;
				select name,
					   strip(name)||" = '"||strip(substr(name, 1, length(name)-2))||"'",
					   "define "||strip(name)||" / center;",
					   strip(strip(substr(name, 1, length(name)-2)))||"_rate_pres = '"||strip(substr(name, 1, length(name)-2))||", n(%)'",
					   strip(strip(substr(name, 1, length(name)-2)))||"_rate_sev = '"||strip(substr(name, 1, length(name)-2))||", n(%)'"
				into : arm_ns separated by " ",
					 : arm_ns_label separated by " ",
					 : arm_ns_center separated by " ",
					 : pres_oth_labs separated by " ",
					 : sev_oth_labs separated by " "
				from ____table_dat_conts
				where substr(name, length(name)-1, 2) = "_n";
				
				select name,
					   "define "||strip(name)||" / center;"
				into : arm_rate_pres separated by " ",
					 : arm_rate_pres_center separated by " "
				from ____table_dat_conts
				where index(name, "_rate_pres")>0;
				
				select name,
					   "define "||strip(name)||" / center;"
				into : arm_rate_sev separated by " ",
					 : arm_rate_sev_center separated by " "
				from ____table_dat_conts
				where index(name, "_rate_sev")>0;
			quit;
		
			/* ----------------------------------------------------------------------------- */
			/* -- Output risk difference tables --- */
			/* ----------------------------------------------------------------------------- */
			%if &arm_count. = 2 and &riskdiff. = 1 %then %do;
				options papersize=(22in 27in);
				ods excel file="&output_dir./&output_filename..xlsx";	
				ods excel options(sheet_name="PRO-CTCAE Individual");
				proc report data=____table_dat_indi_rd style(report)=[frame=void rules=group];
					label label = "PRO-CTCAE Individual Item Analysis" risk_ci_pres = 'Risk difference (95%CI)' 
							risk_ci_sev = 'Risk difference (95%CI)' &arm_ns_label. &pres_oth_labs. &sev_oth_labs. ;
					column label ("N"  &arm_ns.) 
								 ("&adjust_label. >0" &arm_rate_pres. risk_ci_pres) 
								 ("&adjust_label. >=3" &arm_rate_sev. risk_ci_sev);
					&arm_ns_center.	 	 
					&arm_rate_pres_center.
					&arm_rate_sev_center.
					define label / style(column header)={borderrightcolor=black};
					define %sysfunc(scan(&arm_ns.,-1)) / style(column header)={borderrightcolor=black};
					define risk_ci_pres / center style(column header)={borderrightcolor=black};
					define risk_ci_sev / center;
				run;		
				ods excel options(sheet_name="PRO-CTCAE Composite");	
				proc report data=____table_dat_comp_rd style(report)=[frame=void rules=group];
					label label = "PRO-CTCAE Composite Item Analysis" risk_ci_pres = 'Risk difference (95%CI)' 
							risk_ci_sev = 'Risk difference (95%CI)' &arm_ns_label. &pres_oth_labs. &sev_oth_labs. ;
					column label ("N"  &arm_ns.) 
								 ("&adjust_label. >0" &arm_rate_pres. risk_ci_pres) 
								 ("&adjust_label. >=3" &arm_rate_sev. risk_ci_sev);
					&arm_ns_center.	 	 
					&arm_rate_pres_center.
					&arm_rate_sev_center.
					define label / style(column header)={borderrightcolor=black};
					define %sysfunc(scan(&arm_ns.,-1)) / style(column header)={borderrightcolor=black};
					define risk_ci_pres / center style(column header)={borderrightcolor=black};
					define risk_ci_sev / center;
				run;
				ods excel close;
			%end;
			
				/* ----------------------------------------------------------------------------- */
				/* -- Output p value tables --- */
				/* ----------------------------------------------------------------------------- */
				%else %if &riskdiff. ^= 1 %then %do;
					%if %lowcase(%sysfunc(strip("&test."))) = "f" %then %do;
						%let pv_var = fisher;
						%let pv_lab = &pv_var._pres = "Fisher P" &pv_var._sev = "Fisher P";
					%end;			
						%else %if %lowcase(%sysfunc(strip("&test."))) = "c" %then %do;
							%let pv_var = chisqr;
							%let pv_lab = &pv_var._pres = "Chi Square P" &pv_var._sev = "Chi Square P";
						%end;			
					options papersize=(22in 27in);
					ods excel file="&output_dir./&output_filename..xlsx";			
					ods excel options(sheet_name="PRO-CTCAE Individual");
					proc report data=____table_dat_indi style(report)=[frame=void rules=group];
						label label = "PRO-CTCAE Individual Item Analysis" &pv_lab. &arm_ns_label. &pres_oth_labs. &sev_oth_labs. ;
						column label ("N"  &arm_ns.) 
									 ("&adjust_label. >0" &arm_rate_pres. &pv_var._pres) 
									 ("&adjust_label. >=3" &arm_rate_sev. &pv_var._sev);
						&arm_ns_center.	 	 
						&arm_rate_pres_center.
						&arm_rate_sev_center.
						define label / style(column header)={borderrightcolor=black};
						define %sysfunc(scan(&arm_ns.,-1)) / style(column header)={borderrightcolor=black};
						define &pv_var._pres / center style(column header)={font_weight=pv_bold. borderrightcolor=black};
						define &pv_var._sev / center style(column)={font_weight=pv_bold.};
					run;
					ods excel options(sheet_name="PRO-CTCAE Composite");
					proc report data=____table_dat_comp;
						label label = "PRO-CTCAE Composite Item Analysis" &pv_lab. &arm_ns_label. &pres_oth_labs. &sev_oth_labs.;
						column label ("N"  &arm_ns.) 
									 ("&adjust_label. >0" &arm_rate_pres. &pv_var._pres) 
									 ("&adjust_label. >=3" &arm_rate_sev. &pv_var._sev);
						&arm_ns_center.	 
						&arm_rate_pres_center.
						&arm_rate_sev_center.
						define label / style(column header)={borderrightcolor=black};
						define %sysfunc(scan(&arm_ns.,-1)) / style(column header)={borderrightcolor=black};
						define &pv_var._pres / center style(column header)={font_weight=pv_bold. borderrightcolor=black};
						define &pv_var._sev / center style(column)={font_weight=pv_bold.};
					run;
					ods excel close;
				%end;
			%end;
			
	/* ------------------------------ */
	/* --- Clean up ----------------- */
	/* ------------------------------ */
	%exit:
	proc datasets noprint;
		delete ____: _SGSRT2_;
	quit;
%mend;
