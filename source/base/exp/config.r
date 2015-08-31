################################################################################################
################################################################################################

# Installing packages
# install.packages(c("e1071", "PMCMR", "DMwR", "foreign", "caret", "pso"));

# Packages

# SVMs
require("e1071");

# Statistic Tests
require("PMCMR");

# SMOTE balancing
require("DMwR");

# arff files
require("foreign");

# measures / cross validation
require("caret");

# Meta-heuristic packages - PSO
require("pso");

################################################################################################
################################################################################################

# *** Directories/Folders ***

# directory
DIR = getwd();

# home dir (source level)
vect = unlist(strsplit(DIR, "/"))
HOMEDIR = paste(vect[1:which(vect=="source")], collapse="/");

# dataset folder
DATABASE = "/database/";

#output
OUTPUT = "/results/"

# datasets to execute
# SUBDIR = c("normalized/general/");
SUBDIR = c("normalized/evaluation/");

#Algorithm to be optimized
# ALGORITHM = "C45" 
ALGORITHM = "SVM";

# number of folds
FOLDS = 10;

# number of repetitions
# EPOCHS = 30;

#list of heuristics
# HEURISTICS = c("PSO", "RS", "DF");
HEURISTICS = c("RS", "RS");

#list of schedules
SCHEDULE = c(0, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100);

################################################################################################
################################################################################################

# Default values for J48 Classifier
# DF.C45 = c(0.25, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0);

################################################################################################
################################################################################################

# 	- Meta-heuristics parameters

#population size
POP.SIZE = 3 #25 ;

#number of generations without improvement
NOT.CHANGE = 10;

# maximum number of iterations
MAX.ITERATIONS = 2 #100;

#Rounding solutions
ROUNDING = 5;

################################################################################################
################################################################################################
