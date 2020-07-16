
 /*-------------------------------------------------------------------------------------------*
   | MACRO NAME	 :	PROCTCAE_scores
   | VERSION	 :	1.0.0
   | SHORT DESC  :	Recodes PRO-CTCAE survey responses and calculates composite scores
   | 			
   *------------------------------------------------------------------------------------------*
   | AUTHORS  	 :	Blake T Langlais, Amylou C Dueck
   *------------------------------------------------------------------------------------------*
   |				
   *------------------------------------------------------------------------------------------*
   | PURPOSE	 : RECODING
   |			   	This macro takes in a SAS data set with PRO-CTCAE [1] survey text fields/responses 
   |               	and returns a SAS data set with appropriate numerical recoding. This macro 
   |               	will accept 1 or up to all 124 PRO-CTCAE survey fields. All PRO-CTCAE variable
   |               	names MUST conform to a pre-specified naming structure. PRO-CTCAE variable
   |               	names are made up of FOUR components: 1)'PROCTCAE', 2) number [1,2,3, ..., i, ..., 80], 
   |				3) 'A', 'B', or 'C' component of the i-th PRO-CTCAE field, 4) and 'SCL' (if severity,
   |               	interference, or frequency) or 'IND' (if yes/no variable). Each component
   |               	must be delimitated by an underscore (_) 
   |                  EX1: Question 1 of PRO-CTCAE should be: PROCTCAE_1A_SCL
   |                  EX2: Question 48 of PRO-CTCAE should be: PROCTCAE_48A_SCL, PROCTCAE_48B_SCL, PROCTCAE_48C_SCL
   |                  EX3: Question 73 of PRO-CTCAE should be: PROCTCAE_73A_IND
   |				
   |			   COMPOSITE SCORING
   |				This macro also constructs PRO-CTCAE composite scoring [2]. Composite score variables for 
   |				respective PRO-CTCAE item groups are created and named as PROCTCAE_##_COMP.
   |
   |				[1]	https://healthcaredelivery.cancer.gov/pro-ctcae/pro-ctcae_english.pdf
   |				[2] Ethan Basch, et al. Development of a Composite Scoring Algorithm for the 
   |					National Cancer Institute’s Patient-Reported Outcomes version of the Common 
   |					Terminology Criteria for Adverse Events (PRO-CTCAE). ISOQOL 2019.
   |					
   |				EXTPECTED DATA FORMAT
   |				 Data format should be in 'long' format, where each PRO-CTCAE item is a variable/column.
   |				 All PRO-CTCAE variables must either be numeric or character when applying this macro.
   |					
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
	%PROCTCAE_scores(dsn=);
	
	* -- Required and optional parameters;
	%PROCTCAE_scores(dsn=, impute=, dsn_out=, composites=, reformat=);
	
   |
   *------------------------------------------------------------------------------------------*
   | REQUIRED PARAMETERS
   |
   | Name      : dsn
   | Type      : SAS data set name
   | Purpose   : Data set with PRO-CTCAE items and row ID (with optional cycle and arm fields)
   |
   *------------------------------------------------------------------------------------------*
   | OPTIONAL PARAMETERS
   |
   | Name      : reformat
   | Type      : 1 = Reformat PROCTCAE text responses to numeric grades
   |			 0 = Do not reformat (PROCTCAE must be numeric)
   | Purpose   : Reformat PROCTCAE text responses to numeric grades
   | Default   : 0 (Will not reformat text responses)
   |    
   | Name      : impute
   | Type      : 1 = Apply zero-imputation where appropriate
   |			 0 = Will not apply zero-imputation
   | Purpose   : Fills in a '0' score for PROCTCAE_##B/C fields if that PROCTCAE_##B/C is missing and 
   |             the associated PROCTCAE_##A = 0.
   | Default   : 0 (Will not apply zero-imputation)
   |
   | Name      : composites
   | Type      : 1 = Calculate composite scores using available PROCTCAE variables in DSNWill not apply zero-imputation
   |			 0 = Will not calculate composite scores
   | Default   : 0 (Will not calculate composite scores)   
   |
   | Name      : dsn_out 
   | Type      : SAS data set name
   | Purpose   : Creates a new data set copy of dsn with changes from macro application
   | Default   : Output data will take original dsn name (i.e. overwrite original data)
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
   | RETURNED INFORMATION
   |
   |	Returns SAS data set recoded for analysis (with optional imputation and composites)
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

%macro PROCTCAE_scores(dsn, impute, dsn_out, composites, reformat, debug, proctcae_table);

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
	 data ____comp_tab;
		 length comp $1 frq $1 int $1 sev $1;
		 comp='0' ;frq='0' ;int='0' ;sev='0' ; output;
		 comp='0' ;frq='1' ;int='0' ;sev='0' ; output;
		 comp='1' ;frq='1' ;int='1' ;sev='0' ; output;
		 comp='1' ;frq='1' ;int='2' ;sev='0' ; output;
		 comp='2' ;frq='1' ;int='3' ;sev='0' ; output;
		 comp='2' ;frq='1' ;int='4' ;sev='0' ; output;
		 comp='1' ;frq='1' ;int='0' ;sev='1' ; output;
		 comp='1' ;frq='1' ;int='1' ;sev='1' ; output;
		 comp='1' ;frq='1' ;int='2' ;sev='1' ; output;
		 comp='2' ;frq='1' ;int='3' ;sev='1' ; output;
		 comp='2' ;frq='1' ;int='4' ;sev='1' ; output;
		 comp='1' ;frq='1' ;int='0' ;sev='2' ; output;
		 comp='2' ;frq='1' ;int='1' ;sev='2' ; output;
		 comp='2' ;frq='1' ;int='2' ;sev='2' ; output;
		 comp='2' ;frq='1' ;int='3' ;sev='2' ; output;
		 comp='3' ;frq='1' ;int='4' ;sev='2' ; output;
		 comp='2' ;frq='1' ;int='0' ;sev='3' ; output;
		 comp='2' ;frq='1' ;int='1' ;sev='3' ; output;
		 comp='2' ;frq='1' ;int='2' ;sev='3' ; output;
		 comp='3' ;frq='1' ;int='3' ;sev='3' ; output;
		 comp='3' ;frq='1' ;int='4' ;sev='3' ; output;
		 comp='2' ;frq='1' ;int='0' ;sev='4' ; output;
		 comp='2' ;frq='1' ;int='1' ;sev='4' ; output;
		 comp='3' ;frq='1' ;int='2' ;sev='4' ; output;
		 comp='3' ;frq='1' ;int='3' ;sev='4' ; output;
		 comp='3' ;frq='1' ;int='4' ;sev='4' ; output;
		 comp='0' ;frq='2' ;int='0' ;sev='0' ; output;
		 comp='1' ;frq='2' ;int='1' ;sev='0' ; output;
		 comp='1' ;frq='2' ;int='2' ;sev='0' ; output;
		 comp='2' ;frq='2' ;int='3' ;sev='0' ; output;
		 comp='2' ;frq='2' ;int='4' ;sev='0' ; output;
		 comp='1' ;frq='2' ;int='0' ;sev='1' ; output;
		 comp='1' ;frq='2' ;int='1' ;sev='1' ; output;
		 comp='1' ;frq='2' ;int='2' ;sev='1' ; output;
		 comp='2' ;frq='2' ;int='3' ;sev='1' ; output;
		 comp='2' ;frq='2' ;int='4' ;sev='1' ; output;
		 comp='2' ;frq='2' ;int='0' ;sev='2' ; output;
		 comp='2' ;frq='2' ;int='1' ;sev='2' ; output;
		 comp='2' ;frq='2' ;int='2' ;sev='2' ; output;
		 comp='3' ;frq='2' ;int='3' ;sev='2' ; output;
		 comp='3' ;frq='2' ;int='4' ;sev='2' ; output;
		 comp='2' ;frq='2' ;int='0' ;sev='3' ; output;
		 comp='2' ;frq='2' ;int='1' ;sev='3' ; output;
		 comp='2' ;frq='2' ;int='2' ;sev='3' ; output;
		 comp='3' ;frq='2' ;int='3' ;sev='3' ; output;
		 comp='3' ;frq='2' ;int='4' ;sev='3' ; output;
		 comp='2' ;frq='2' ;int='0' ;sev='4' ; output;
		 comp='2' ;frq='2' ;int='1' ;sev='4' ; output;
		 comp='3' ;frq='2' ;int='2' ;sev='4' ; output;
		 comp='3' ;frq='2' ;int='3' ;sev='4' ; output;
		 comp='3' ;frq='2' ;int='4' ;sev='4' ; output;
		 comp='1' ;frq='3' ;int='0' ;sev='0' ; output;
		 comp='1' ;frq='3' ;int='1' ;sev='0' ; output;
		 comp='1' ;frq='3' ;int='2' ;sev='0' ; output;
		 comp='2' ;frq='3' ;int='3' ;sev='0' ; output;
		 comp='2' ;frq='3' ;int='4' ;sev='0' ; output;
		 comp='1' ;frq='3' ;int='0' ;sev='1' ; output;
		 comp='1' ;frq='3' ;int='1' ;sev='1' ; output;
		 comp='1' ;frq='3' ;int='2' ;sev='1' ; output;
		 comp='2' ;frq='3' ;int='3' ;sev='1' ; output;
		 comp='2' ;frq='3' ;int='4' ;sev='1' ; output;
		 comp='2' ;frq='3' ;int='0' ;sev='2' ; output;
		 comp='2' ;frq='3' ;int='1' ;sev='2' ; output;
		 comp='2' ;frq='3' ;int='2' ;sev='2' ; output;
		 comp='3' ;frq='3' ;int='3' ;sev='2' ; output;
		 comp='3' ;frq='3' ;int='4' ;sev='2' ; output;
		 comp='2' ;frq='3' ;int='0' ;sev='3' ; output;
		 comp='2' ;frq='3' ;int='1' ;sev='3' ; output;
		 comp='3' ;frq='3' ;int='2' ;sev='3' ; output;
		 comp='3' ;frq='3' ;int='3' ;sev='3' ; output;
		 comp='3' ;frq='3' ;int='4' ;sev='3' ; output;
		 comp='2' ;frq='3' ;int='0' ;sev='4' ; output;
		 comp='2' ;frq='3' ;int='1' ;sev='4' ; output;
		 comp='3' ;frq='3' ;int='2' ;sev='4' ; output;
		 comp='3' ;frq='3' ;int='3' ;sev='4' ; output;
		 comp='3' ;frq='3' ;int='4' ;sev='4' ; output;
		 comp='1' ;frq='4' ;int='0' ;sev='0' ; output;
		 comp='1' ;frq='4' ;int='1' ;sev='0' ; output;
		 comp='1' ;frq='4' ;int='2' ;sev='0' ; output;
		 comp='2' ;frq='4' ;int='3' ;sev='0' ; output;
		 comp='2' ;frq='4' ;int='4' ;sev='0' ; output;
		 comp='1' ;frq='4' ;int='0' ;sev='1' ; output;
		 comp='1' ;frq='4' ;int='1' ;sev='1' ; output;
		 comp='2' ;frq='4' ;int='2' ;sev='1' ; output;
		 comp='2' ;frq='4' ;int='3' ;sev='1' ; output;
		 comp='3' ;frq='4' ;int='4' ;sev='1' ; output;
		 comp='2' ;frq='4' ;int='0' ;sev='2' ; output;
		 comp='2' ;frq='4' ;int='1' ;sev='2' ; output;
		 comp='2' ;frq='4' ;int='2' ;sev='2' ; output;
		 comp='3' ;frq='4' ;int='3' ;sev='2' ; output;
		 comp='3' ;frq='4' ;int='4' ;sev='2' ; output;
		 comp='2' ;frq='4' ;int='0' ;sev='3' ; output;
		 comp='2' ;frq='4' ;int='1' ;sev='3' ; output;
		 comp='3' ;frq='4' ;int='2' ;sev='3' ; output;
		 comp='3' ;frq='4' ;int='3' ;sev='3' ; output;
		 comp='3' ;frq='4' ;int='4' ;sev='3' ; output;
		 comp='2' ;frq='4' ;int='0' ;sev='4' ; output;
		 comp='2' ;frq='4' ;int='1' ;sev='4' ; output;
		 comp='3' ;frq='4' ;int='2' ;sev='4' ; output;
		 comp='3' ;frq='4' ;int='3' ;sev='4' ; output;
		 comp='3' ;frq='4' ;int='4' ;sev='4' ; output;
		 comp='0' ;frq='0' ;int='' ;sev='0' ; output;
		 comp='1' ;frq='1' ;int='' ;sev='0' ; output;
		 comp='1' ;frq='1' ;int='' ;sev='1' ; output;
		 comp='1' ;frq='1' ;int='' ;sev='2' ; output;
		 comp='2' ;frq='1' ;int='' ;sev='3' ; output;
		 comp='2' ;frq='1' ;int='' ;sev='4' ; output;
		 comp='1' ;frq='2' ;int='' ;sev='0' ; output;
		 comp='1' ;frq='2' ;int='' ;sev='1' ; output;
		 comp='2' ;frq='2' ;int='' ;sev='2' ; output;
		 comp='2' ;frq='2' ;int='' ;sev='3' ; output;
		 comp='2' ;frq='2' ;int='' ;sev='4' ; output;
		 comp='1' ;frq='3' ;int='' ;sev='0' ; output;
		 comp='1' ;frq='3' ;int='' ;sev='1' ; output;
		 comp='2' ;frq='3' ;int='' ;sev='2' ; output;
		 comp='3' ;frq='3' ;int='' ;sev='3' ; output;
		 comp='3' ;frq='3' ;int='' ;sev='4' ; output;
		 comp='1' ;frq='4' ;int='' ;sev='0' ; output;
		 comp='1' ;frq='4' ;int='' ;sev='1' ; output;
		 comp='2' ;frq='4' ;int='' ;sev='2' ; output;
		 comp='3' ;frq='4' ;int='' ;sev='3' ; output;
		 comp='3' ;frq='4' ;int='' ;sev='4' ; output;
		 comp='0' ;frq='' ;int='0' ;sev='0' ; output;
		 comp='1' ;frq='' ;int='0' ;sev='1' ; output;
		 comp='1' ;frq='' ;int='1' ;sev='1' ; output;
		 comp='1' ;frq='' ;int='2' ;sev='1' ; output;
		 comp='2' ;frq='' ;int='3' ;sev='1' ; output;
		 comp='2' ;frq='' ;int='4' ;sev='1' ; output;
		 comp='1' ;frq='' ;int='0' ;sev='2' ; output;
		 comp='1' ;frq='' ;int='1' ;sev='2' ; output;
		 comp='2' ;frq='' ;int='2' ;sev='2' ; output;
		 comp='2' ;frq='' ;int='3' ;sev='2' ; output;
		 comp='3' ;frq='' ;int='4' ;sev='2' ; output;
		 comp='1' ;frq='' ;int='0' ;sev='3' ; output;
		 comp='2' ;frq='' ;int='1' ;sev='3' ; output;
		 comp='2' ;frq='' ;int='2' ;sev='3' ; output;
		 comp='3' ;frq='' ;int='3' ;sev='3' ; output;
		 comp='3' ;frq='' ;int='4' ;sev='3' ; output;
		 comp='2' ;frq='' ;int='0' ;sev='4' ; output;
		 comp='2' ;frq='' ;int='1' ;sev='4' ; output;
		 comp='2' ;frq='' ;int='2' ;sev='4' ; output;
		 comp='3' ;frq='' ;int='3' ;sev='4' ; output;
		 comp='3' ;frq='' ;int='4' ;sev='4' ; output;
		 comp='0' ;frq='0' ;int='0' ;sev='' ; output;
		 comp='1' ;frq='1' ;int='0' ;sev='' ; output;
		 comp='1' ;frq='1' ;int='1' ;sev='' ; output;
		 comp='1' ;frq='1' ;int='2' ;sev='' ; output;
		 comp='2' ;frq='1' ;int='3' ;sev='' ; output;
		 comp='2' ;frq='1' ;int='4' ;sev='' ; output;
		 comp='1' ;frq='2' ;int='0' ;sev='' ; output;
		 comp='1' ;frq='2' ;int='1' ;sev='' ; output;
		 comp='1' ;frq='2' ;int='2' ;sev='' ; output;
		 comp='2' ;frq='2' ;int='3' ;sev='' ; output;
		 comp='2' ;frq='2' ;int='4' ;sev='' ; output;
		 comp='1' ;frq='3' ;int='0' ;sev='' ; output;
		 comp='1' ;frq='3' ;int='1' ;sev='' ; output;
		 comp='2' ;frq='3' ;int='2' ;sev='' ; output;
		 comp='3' ;frq='3' ;int='3' ;sev='' ; output;
		 comp='3' ;frq='3' ;int='4' ;sev='' ; output;
		 comp='1' ;frq='4' ;int='0' ;sev='' ; output;
		 comp='1' ;frq='4' ;int='1' ;sev='' ; output;
		 comp='2' ;frq='4' ;int='2' ;sev='' ; output;
		 comp='3' ;frq='4' ;int='3' ;sev='' ; output;
		 comp='3' ;frq='4' ;int='4' ;sev='' ; output;
		 comp='0' ;frq='0' ;int='' ;sev='' ; output;
		 comp='1' ;frq='1' ;int='' ;sev='' ; output;
		 comp='1' ;frq='2' ;int='' ;sev='' ; output;
		 comp='2' ;frq='3' ;int='' ;sev='' ; output;
		 comp='3' ;frq='4' ;int='' ;sev='' ; output;
		 comp='0' ;frq='' ;int='' ;sev='0' ; output;
		 comp='1' ;frq='' ;int='' ;sev='1' ; output;
		 comp='2' ;frq='' ;int='' ;sev='2' ; output;
		 comp='3' ;frq='' ;int='' ;sev='3' ; output;
		 comp='3' ;frq='' ;int='' ;sev='4' ; output;
		 comp='0' ;frq='' ;int='0' ;sev='' ; output;
		 comp='1' ;frq='' ;int='1' ;sev='' ; output;
		 comp='1' ;frq='' ;int='2' ;sev='' ; output;
		 comp='2' ;frq='' ;int='3' ;sev='' ; output;
		 comp='2' ;frq='' ;int='4' ;sev='' ; output;
	run;
	data ____map_ref0;
		 length fmt $100 qset $100 whr $100;
		 fmt='frq' ;qset='16 67' ;whr='frq ^="" and sev ="" and int =""' ; output;
		 fmt='sev' ;qset='1 2 4 6 7 15 21 25 26 28 30 36 45 60 61 66 68 71 72 80' ;whr='frq ="" and sev ^="" and int =""' ; output;
		 fmt='int' ;qset='27 59' ;whr='frq ="" and sev ="" and int ^=""' ; output;
		 fmt='frq sev' ;qset='9 10 11 13 14 23 74 75 77 78' ;whr='frq ^="" and sev ^="" and int =""' ; output;
		 fmt='frq int' ;qset='18 62 63 65' ;whr='frq ^="" and sev ="" and int ^=""' ; output;
		 fmt='sev int' ;qset='3 8 19 20 39 40 41 44 46 47 52 53' ;whr='frq ="" and sev ^="" and int ^=""' ; output;
		 fmt='frq sev int' ;qset='17 22 48 49 50 51 54 55 56' ;whr='frq ^="" and sev ^="" and int ^=""' ; output;
 	run;
 	
 	%if %length(&reformat.) = 0 %then %do;
		%let reformat = 0;
	%end;
	
	/* ---------------------------------------------------------------------------------------------------- */	
	/* --- Error checks (1 of 3) --- */
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

	/* ---------------------------------------------------------------------------------------------------- */	
	/* --- Create new data as to not overwrite user's orig data */
	/* ---------------------------------------------------------------------------------------------------- */	
	data ____dsn1;
		set &dsn.;
	run;
	proc contents data=____dsn1 noprint out=____dsn_conts;
	run;
	proc sql noprint;
		select ____dsn_conts.name || "=" || upcase(____dsn_conts.name)
		into : upcase_rename separated by " "
		from ____dsn_conts, ____proctcae_vars
		where upcase(____dsn_conts.name) = ____proctcae_vars.name;
	quit;
	data ____dsn1;
		set ____dsn1(rename=(&upcase_rename.));
	run;
	proc contents data=____dsn1 noprint out=____dsn_conts;
	run;
	
	/* ---------------------------------------------------------------------------------------------------- */	
	/* --- Error checks (2 of 3) --- */
	/* ---------------------------------------------------------------------------------------------------- */	
	proc sql noprint;
		select "'"||strip(upcase(name))||"'"
		into : dsn_pro_vars separated by " "
		from ____dsn_conts;
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
	/* --- Defaults / references --- */
	/* ---------------------------------------------------------------------------------------------------- */	
	proc format;
		invalue int_5_fmt (upcase)
			'NOT AT ALL' = 0
			'A LITTLE BIT' = 1
			'SOMEWHAT' = 2
			'QUITE A BIT' = 3
			'VERY MUCH' = 4
			' ' = .
			other = 100;
		invalue sev_6_fmt (upcase)
			'NONE' = 0
			'MILD' = 1
			'MODERATE' = 2
			'SEVERE' = 3
			'VERY SEVERE' = 4
			'NOT APPLICABLE' = .
			' ' = .
			other = 100;;
		invalue sev_5_fmt (upcase)
			'NONE' = 0
			'MILD' = 1
			'MODERATE' = 2
			'SEVERE' = 3
			'VERY SEVERE' = 4
			' ' = .
			other = 100;;
		invalue sev_7_fmt (upcase)
			'NONE' = 0
			'MILD' = 1
			'MODERATE' = 2
			'SEVERE' = 3
			'VERY SEVERE' = 4
			'NOT SEXUALLY ACTIVE' = .
			'PREFER NOT TO ANSWER' = .
			' ' = .
			other = 100;;
		invalue frq_5_fmt (upcase)
			'NEVER' = 0
			'RARELY' = 1
			'OCCASIONALLY' = 2
			'FREQUENTLY' = 3
			'ALMOST CONSTANTLY' = 4
			' ' = .
			other = 100;;
		invalue frq_7_fmt (upcase)
			'NEVER' = 0
			'RARELY' = 1
			'OCCASIONALLY' = 2
			'FREQUENTLY' = 3
			'ALMOST CONSTANTLY' = 4
			'NOT SEXUALLY ACTIVE' = .
			'PREFER NOT TO ANSWER' = .
			' ' = .
			other = 100;;
		invalue yn_2_fmt (upcase)
			'YES' = 1
			'NO' = 0
			' ' = .
			other = 100;;
		invalue yn_3_fmt (upcase)
			'YES' = 1
			'NO' = 0
			'NOT APPLICABLE' = .
			' ' = .
			other = 100;;
		invalue yn_4_fmt (upcase)
			'YES' = 1
			'NO' = 0
			'NOT SEXUALLY ACTIVE' = .
			'PREFER NOT TO ANSWER' = .
			' ' = .
			other = 100;;
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
		name = "PROCTCAE_"||strip(num)||"_COMP";
	run;
	%if &proctcae_table.=1 %then %do;
		data PROCTCAE_table;
			set ____proctcae_vars (drop=fmt_name);
		run;
	%end;
		
	/* ---------------------------------------------------------------------------------------------------- */	
	/* --- Construct code to later impute zeros of of q's with A/B or A/B/C structure */
	/* ---------------------------------------------------------------------------------------------------- */
	/* --- Write a check to confirm if there is a "B" or "C" field, the associated "A" field exists */
	%let impute0_AB=;
	%let impute0_AC=;
	proc sql noprint;
		select "if PROCTCAE_"||strip(compress(name,,"kd"))||"A_"||substr(strip(name), length(name)-2, 3)||" = 0 and "||strip(name)||"=. then "||strip(name)||"=0"
		into : impute0_AB separated by "; "
		from ____dsn_conts
		where index(upcase(name), "PROCTCAE") > 0 and index(scan(name, 2, "_"), "B") > 0;
		select "if PROCTCAE_"||strip(compress(name,,"kd"))||"A_"||substr(strip(name), length(name)-2, 3)||" = 0 and "||strip(name)||"=. then "||strip(name)||"=0"
		into : impute0_AC separated by "; "
		from ____dsn_conts
		where index(upcase(name), "PROCTCAE") > 0 and index(scan(name, 2, "_"), "C") > 0;
	quit;
	
	/* ---------------------------------------------------------------------------------------------------- */	
	/* --- Construct code to later write label statements for existing PROCTCAEs in the dsn */
	/* ---------------------------------------------------------------------------------------------------- */
	proc sql noprint;
		select ____proctcae_vars.name || " ='" || strip(____proctcae_vars.short_label) ||"'"
		into : labels separated by " "
		from ____dsn_conts, ____proctcae_vars
		where upcase(____dsn_conts.name) = ____proctcae_vars.name;
		select ____proctcae_vars.name, "old_"||strip(____proctcae_vars.name), "'"||strip(____proctcae_vars.short_label)||"'", count(____proctcae_vars.short_label)
		into : PROCTCAE_reorder separated by " ",
			 : old_PROCTCAE_reorder separated by " ",
			 : PROCTCAE_reorder_lab separated by " ",
			 : PROCTCAE_reorder_lab_count
		from ____dsn_conts, ____proctcae_vars
		where upcase(____dsn_conts.name) = ____proctcae_vars.name
		order by name;
	quit;

	/* ---------------------------------------------------------------------------------------------------- */	
	/* --- Partition existing PROCTCAEs in dsn into respective lists according to response type */
	/* ---------------------------------------------------------------------------------------------------- */	
	%let int_5_vars=;
	%let sev_5_vars=;
	%let sev_6_vars=;
	%let sev_7_vars=;
	%let frq_5_vars=;
	%let frq_7_vars=;
	%let yn_2_vars=;
	%let yn_3_vars=;
	%let yn_4_vars=;
	proc sql noprint;
		select ____proctcae_vars.name
		into : int_5_vars separated by " "
		from ____dsn_conts, ____proctcae_vars
		where ____dsn_conts.name = ____proctcae_vars.name and ____proctcae_vars.fmt_name = "int_5_fmt";
		
		select ____proctcae_vars.name
		into : sev_5_vars separated by " "
		from ____dsn_conts, ____proctcae_vars
		where ____dsn_conts.name = ____proctcae_vars.name and ____proctcae_vars.fmt_name = "sev_5_fmt";
		
		select ____proctcae_vars.name
		into : sev_6_vars separated by " "
		from ____dsn_conts, ____proctcae_vars
		where ____dsn_conts.name = ____proctcae_vars.name and ____proctcae_vars.fmt_name = "sev_6_fmt";
		
		select ____proctcae_vars.name
		into : sev_7_vars separated by " "
		from ____dsn_conts, ____proctcae_vars
		where ____dsn_conts.name = ____proctcae_vars.name and ____proctcae_vars.fmt_name = "sev_7_fmt";
		
		select ____proctcae_vars.name
		into : frq_5_vars separated by " "
		from ____dsn_conts, ____proctcae_vars
		where ____dsn_conts.name = ____proctcae_vars.name and ____proctcae_vars.fmt_name = "frq_5_fmt";
		
		select ____proctcae_vars.name
		into : frq_7_vars separated by " "
		from ____dsn_conts, ____proctcae_vars
		where ____dsn_conts.name = ____proctcae_vars.name and ____proctcae_vars.fmt_name = "frq_7_fmt";

		select ____proctcae_vars.name
		into : yn_2_vars separated by " "
		from ____dsn_conts, ____proctcae_vars
		where ____dsn_conts.name = ____proctcae_vars.name and ____proctcae_vars.fmt_name = "yn_2_fmt";
		
		select ____proctcae_vars.name
		into : yn_3_vars separated by " "
		from ____dsn_conts, ____proctcae_vars
		where ____dsn_conts.name = ____proctcae_vars.name and ____proctcae_vars.fmt_name = "yn_3_fmt";

		select ____proctcae_vars.name
		into : yn_4_vars separated by " "
		from ____dsn_conts, ____proctcae_vars
		where ____dsn_conts.name = ____proctcae_vars.name and ____proctcae_vars.fmt_name = "yn_4_fmt";
	quit;
	%let old_int_5_vars= %sysfunc(prxchange(s/(\w+)/old_$1/,-1,&int_5_vars.));
	%let old_sev_5_vars= %sysfunc(prxchange(s/(\w+)/old_$1/,-1,&sev_5_vars.));
	%let old_sev_6_vars= %sysfunc(prxchange(s/(\w+)/old_$1/,-1,&sev_6_vars.));
	%let old_sev_7_vars= %sysfunc(prxchange(s/(\w+)/old_$1/,-1,&sev_7_vars.));
	%let old_frq_5_vars= %sysfunc(prxchange(s/(\w+)/old_$1/,-1,&frq_5_vars.));
	%let old_frq_7_vars= %sysfunc(prxchange(s/(\w+)/old_$1/,-1,&frq_7_vars.));
	%let old_yn_2_vars= %sysfunc(prxchange(s/(\w+)/old_$1/,-1,&yn_2_vars.));
	%let old_yn_3_vars= %sysfunc(prxchange(s/(\w+)/old_$1/,-1,&yn_3_vars.));
	%let old_yn_4_vars= %sysfunc(prxchange(s/(\w+)/old_$1/,-1,&yn_4_vars.));
	
	/* ---------------------------------------------------------------------------------------------------- */	
	/* --- Add prefixes to all PRO-CTCAE Q's for later reformating */
	/* ---------------------------------------------------------------------------------------------------- */
	%let keepAsIs =;
	%let keepOrder =;
	proc sql noprint;
		select "'"||strip(lowcase(name))||"'"
		into : keepAsIs separated by " "
		from ____dsn_conts
		where index(lowcase(name), "proctcae_") = 0;
		
		select lowcase(name)
		into : keepOrder separated by " "
		from ____dsn_conts
		where index(lowcase(name), "proctcae_") = 0
		order by varnum;
	quit;
	proc sql noprint;
		select upcase(strip(name))||strip("=old_")||upcase(strip(name))
		into : ad_prfixs separated by " "
		from ____dsn_conts
		where substr(lowcase(name),1,length(strip("old_")))^=lowcase(strip("old_"))
				%if %length(&keepAsIs.)>0 %then %do;
					and lowcase(name) ^ in (&keepAsIs.);;
				%end;;
	quit;
	proc datasets lib=work noprint;
		modify ____dsn1;
		rename &ad_prfixs.;
	quit;
	
	/* ---------------------------------------------------------------------------------------------------- */	
	/* --- Reformat and apply optional zero imputation
	/* ---------------------------------------------------------------------------------------------------- */
	%let bad_obs=;
	data ____dsn2;
		set ____dsn1;	
		%if %length(&int_5_vars.)>0 %then %do;
			array int_5(*) &int_5_vars.;
			array int_5_old(*) &old_int_5_vars.;
			do ____i=1 to dim(int_5);
				%if &reformat. = 1 %then %do;
					int_5(____i) = input(upcase(strip(int_5_old(____i))), int_5_fmt.);
				%end;
					%else %if &reformat. ^= 1 %then %do;
				 		int_5(____i) = int_5_old(____i);
				 	%end;
			end;
		%end;
		%if %length(&sev_5_vars.)>0 %then %do;
			array sev_5(*) &sev_5_vars.;
			array sev_5_old(*) &old_sev_5_vars.;
			do ____i=1 to dim(sev_5);
				%if &reformat. = 1 %then %do;
					sev_5(____i) = input(upcase(strip(sev_5_old(____i))), sev_5_fmt.);
				%end;
					%else %if &reformat. ^= 1 %then %do;
				 		sev_5(____i) = sev_5_old(____i);
				 	%end;
			end;
		%end;
		%if %length(&sev_6_vars.)>0 %then %do;
			array sev_6(*) &sev_6_vars.;
			array sev_6_old(*) &old_sev_6_vars.;
			do ____i=1 to dim(sev_6);
				%if &reformat. = 1 %then %do;
					sev_6(____i) = input(upcase(strip(sev_6_old(____i))), sev_6_fmt.);
				%end;
					%else %if &reformat. ^= 1 %then %do;
				 		sev_6(____i) = sev_6_old(____i);
				 	%end;
			end;
		%end;
		%if %length(&sev_7_vars.)>0 %then %do;
			array sev_7(*) &sev_7_vars.;
			array sev_7_old(*) &old_sev_7_vars.;
			do ____i=1 to dim(sev_7);
				%if &reformat. = 1 %then %do;
					sev_7(____i) = input(upcase(strip(sev_7_old(____i))), sev_7_fmt.);
				%end;
					%else %if &reformat. ^= 1 %then %do;
				 		sev_7(____i) = sev_7_old(____i);
				 	%end;
			end;
		%end;
		%if %length(&frq_5_vars.)>0 %then %do;
			array frq_5(*) &frq_5_vars.;
			array frq_5_old(*) &old_frq_5_vars.;
			do ____i=1 to dim(frq_5);
				%if &reformat. = 1 %then %do;				
					frq_5(____i) = input(upcase(strip(frq_5_old(____i))), frq_5_fmt.);
				%end;
					%else %if &reformat. ^= 1 %then %do;
				 		frq_5(____i) = frq_5_old(____i);
				 	%end;
			end;
		%end;
		%if %length(&frq_7_vars.)>0 %then %do;
			array frq_7(*) &frq_7_vars.;
			array frq_7_old(*) &old_frq_7_vars.;
			do ____i=1 to dim(frq_7);
				%if &reformat. = 1 %then %do;				
					frq_7(____i) = input(upcase(strip(frq_7_old(____i))), frq_7_fmt.);
				%end;
					%else %if &reformat. ^= 1 %then %do;
				 		frq_7(____i) = frq_7_old(____i);
				 	%end;
			end;
		%end;
		%if %length(&yn_2_vars.)>0 %then %do;
			array yn_2(*) &yn_2_vars.;
			array yn_2_old(*) &old_yn_2_vars.;
			do ____i=1 to dim(yn_2);
				%if &reformat. = 1 %then %do;				
					yn_2(____i) = input(upcase(strip(yn_2_old(____i))), yn_2_fmt.);
				%end;
					%else %if &reformat. ^= 1 %then %do;
				 		yn_2(____i) = yn_2_old(____i);
				 	%end;
			end;
		%end;
		%if %length(&yn_3_vars.)>0 %then %do;
			array yn_3(*) &yn_3_vars.;
			array yn_3_old(*) &old_yn_3_vars.;
			do ____i=1 to dim(yn_3);
				%if &reformat. = 1 %then %do;				
					yn_3(____i) = input(upcase(strip(yn_3_old(____i))), yn_3_fmt.);
				%end;
					%else %if &reformat. ^= 1 %then %do;
				 		yn_3(____i) = yn_3_old(____i);
				 	%end;
			end;
		%end;
		%if %length(&yn_4_vars.)>0 %then %do;
			array yn_4(*) &yn_4_vars.;
			array yn_4_old(*) &old_yn_4_vars.;
			do ____i=1 to dim(yn_4);
				%if &reformat. = 1 %then %do;				
					yn_4(____i) = input(upcase(strip(yn_4_old(____i))), yn_4_fmt.);
				%end;
					%else %if &reformat. ^= 1 %then %do;
				 		yn_4(____i) = yn_4_old(____i);
				 	%end;
			end;
		%end;
		%if &impute. = 1 %then %do;
			&impute0_AB.;
			&impute0_AC.;
		%end;
		%if %length(&labels.)>0 %then %do;
			label &labels.;
		%end;
		
		/* ---------------------------------------------------------------------------------------------------- */	
		/* --- Error checks (3 of 3) --- */
		/* ---------------------------------------------------------------------------------------------------- */
		%if &reformat. = 1 %then %do;
			array provars(*) &PROCTCAE_reorder.;
			array provars_old(*) &old_PROCTCAE_reorder.;
			array label(&PROCTCAE_reorder_lab_count.) $50 _temporary_ (&PROCTCAE_reorder_lab.);
			do i=1 to dim(provars);
				if provars(i)=100 then do;
					call symput("bad_obs", 1);
					_obs_number_ = _n_;
					put "ER" "ROR: The text response observed for this PRO-CTCAE item is unexpected.";
					put "ER" "ROR: See observation number and unexpected PRO-CTCAE response below.";
					put _obs_number_=;
					put label(i)=;
					put provars_old(i)=;
				end;
			end;
		%end;
			%else %if &reformat. = 0 %then %do;
				array provars(*) &PROCTCAE_reorder.;
				array label(&PROCTCAE_reorder_lab_count.) $50 _temporary_ (&PROCTCAE_reorder_lab.);
				do i=1 to dim(provars);
					if provars(i) ^ in (.,0,1,2,3,4) then do;
						call symput("bad_obs", 1);
						_obs_number_ = _n_;
						put "ER" "ROR: Numerical PRO-CTCAE item responses should be integers between 0 and 4.";
						put "ER" "ROR: See observation number and unexpected PRO-CTCAE response below.";
						put _obs_number_=;
						put label(i)=;
						put provars(i)=;
					end;
				end;
			%end;

		/* ---------------------------------------------------------------------------------------------------- */
		/* --- --- */
		/* ---------------------------------------------------------------------------------------------------- */	

		drop ____i &old_int_5_vars. &old_sev_5_vars. &old_sev_6_vars. &old_sev_7_vars. &old_frq_5_vars. 
			&old_frq_7_vars. &old_yn_2_vars. &old_yn_3_vars. &old_yn_4_vars. old_proctcae_: i _obs_number_;	
	run;
	%if &bad_obs.=1 %then %do;
		%goto exit;
	%end;
	%if %length(&dsn_out.)=0 %then %do;
		%let dsn_out = &dsn.;
    %end;
	data &dsn_out.;
		retain &keepOrder. &PROCTCAE_reorder.;
		set ____dsn2;
	run;
	
	/* --------------------------------------------------------------------------------------------- */
	/* --- Composite scoring */
	/* --------------------------------------------------------------------------------------------- */
	%if &composites. = 1 %then %do;
		data ____conts0;
			set ____proctcae_vars;
			qnum = input(compress(name,, "kd"), best8.);
		run;
		data ____conts1;
			set ____conts0;
			length fmt_list $30;
			by qnum;
			retain fmt_list;
			if first.qnum then do;
				rank = 0;
				fmt_list = "";
			end;
			fmt_list = catx(" ", fmt_list, fmt_name);
			rank + 1; 
		run;
		proc sql noprint;
			create table ____conts2 as
			select *, max(rank) as item_nums
			from ____conts1
			group by qnum;
		quit;
		data ____map_ref;
			set ____map_ref0 end = last_rank;
			rank + 1;
			if last_rank then do;
				call symput("max_map", rank);
			end;
		run;
		%let i = 1;
		%do %while(&i. <= &max_map.);
			/* --------------------------------------------------------------------------------------------- */
			/* --- Get the the ith mapping set - */
			/* --------------------------------------------------------------------------------------------- */
			data _null_;                                                                                                                       
				set ____map_ref (where=(rank = &i.));
				call symput("qset", qset);
				call symput("fmt", fmt);
				call symput("whr", whr);
			run;
			data ____map_i;
				set ____conts2 (rename=(fmt_name = temp));
				fmt_name = substr(temp, 1,3);
				if qnum in (&qset.);
				drop temp;
			run;
			proc sort data=____map_i;
				by fmt_name;
			run;
			proc transpose data=____map_i(keep=name fmt_name) out=____map_i_trans0;
				var name;
				by fmt_name;
			run;
			proc transpose data=____map_i_trans0(drop=_name_) out=____map_i_trans(drop=_name_);
				var col:;
				id fmt_name;
			run;
			data ____comps(keep = &fmt. comp);
				retain frq sev int comp;
				set ____comp_tab;
				where  &whr.;
			run;
			
			/* --------------------------------------------------------------------------------------------- */
			/* --- Create a reference data set */
			/* --------------------------------------------------------------------------------------------- */
			proc contents data=____map_i_trans out=____tmp_conts noprint;
			run;
			data _null_;
				set ____tmp_conts;
				if _n_ = 1 then do;
					call symput("first_fmt_var", name);
				end;
			run;
			data ____vars_ref;                                                                                                                       
				set ____map_i_trans end = last_rank;
				qnum = compress(&first_fmt_var.,,"kd");
				rank + 1;
				if last_rank then do;
					call symput("max_rank", rank);
				end;
			run;
			%let j = 1;
			%do %while(&j. <= &max_rank.);
			
				/* --------------------------------------------------------------------------------------------- */
				/* --- Get the each of the ith PROCTCAE A/B/C field names from the ref list to calc comp score - */
				/* --------------------------------------------------------------------------------------------- */
				proc transpose data=____vars_ref (where=(rank = &j.)) out=____vars_ref_trans(where=(_name_ ^ in ("rank")));
					var _all_;
				run;
				%let frq=;
				%let sev=;
				%let int=;
				data _null_;
					set ____vars_ref_trans;
					call symputx(_name_,col1);
				run;
	
				/* --------------------------------------------------------------------------------------------- */
				/* --- Check if dsn has each of the required fields to calc comp score --- */
				/* --------------------------------------------------------------------------------------------- */
				data ____check_list(keep=ques);
					set ____vars_ref_trans(where=(_name_^="qnum"));
					rename col1 = ques;
				run;
				proc sql noprint;
					create table ____check_list2 as 
					select upcase(ques) as ques 
					from ____check_list except 
						select upcase(name) from ____dsn_conts;
					select count(*) into : all_ques_flag from ____check_list2;
				quit;
	
				/* --------------------------------------------------------------------------------------------- */
				/* --- If dsn has all require fields to calc comp score then create and apply comp coding --- */
				/* --------------------------------------------------------------------------------------------- */
				%if &all_ques_flag. = 0  %then %do;
					data ____comps_asd;
						set ____comps;
						length full_coding $256;
						%if %length(&frq.)>0 %then %do;
							code_frq = "&frq. = "||strip(frq);
						%end;
							%else %do; 
							code_frq = " "; 
							%end;
						%if %length(&sev.)>0 %then %do;
							code_sev = "&sev. = "||strip(sev);
						%end;
							%else %do; 
							code_sev = " "; 
							%end;
						%if %length(&int.)>0 %then %do;
							code_int = "&int. = "||strip(int);
						%end;
							%else %do; 
							code_int = " ";
							%end;
						full_coding = "else if "||catx(" and ", code_frq, code_sev, code_int)||" then PROCTCAE_&qnum._COMP = "||strip(comp)|| ";";
					run;
					proc sql noprint;
						select full_coding
						into : composite_coding separated by " "
						from ____comps_asd;
						
						select name ||"= "||"'"||strip(comp_label)||"'"
						into : composite_label
						from ____proctcae_comp_vars
						where lowcase(name) = "proctcae_&qnum._comp";
					quit;
					data &dsn_out.;
						set &dsn_out.;
						if nmiss(of &frq. &sev. &int.) = 0 then do;
							/* -- If a freq item included in the composite and is 0, then comp score = 0 */
							%if %length(&frq.)>0 %then %do;
								if &frq. = 0  then PROCTCAE_&qnum._COMP = 0;
							%end;
								/* -- If no freq item included, but there is a sev item in the composite and is 0, then comp score = 0 */
								%else %if %length(&sev.)>0 %then %do;
									if &sev. = 0  then PROCTCAE_&qnum._COMP = 0;
								%end;
									/* -- If no freq/sev item included (i.e. only an int item), and the int item is 0, then comp score = 0 */
									%else %if %length(&int.)>0 %then %do;
										if &int. = 0  then PROCTCAE_&qnum._COMP = 0;
									%end;
						/*else...*/
						&composite_coding.;
						label &composite_label.;
						end;
					run;
					%end;				
			%let j = %eval(&j.+1);
			%end;	
		%let i = %eval(&i.+1);
		%end;
	%end;
	
	/* ------------------------------ */
	/* --- Clean up ----------------- */
	/* ------------------------------ */
	%exit:
	proc datasets noprint nolist; 
		delete ____:;
	quit;
	options &user_notes. &user_mprint. &user_symbolgen. &user_mlogic. &user_mlogicnest.;
%mend;
