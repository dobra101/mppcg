.SUFFIXES:

.PHONY: all clean

all: CAN_BUS_tlc Cruise_finite1_deterministic_MC LandingGear_R6 Lift nota_v2 QueensWithEvents_4 QueensWithEvents_8 scheduler_deterministic_MC sort_m2_data1000 Train1_Lukas_POR Train_1_beebook_deterministic_MC
PROBCLI=./probcli.sh
FLAGS=--model-check -disable-time-out -pref_group model_check unlimited -p COMPRESSION TRUE -noass -memory
OUTPUT ?= runtimes.txt

% :
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
	timeout 60m gtime -f "%C %E %M" -ao $(OUTPUT) $(PROBCLI) machines/$@.P $(FLAGS)
