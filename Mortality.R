#With RNN, the prediction is based not only on the properties at the current time,
#Therefore, RNN is particularly effective when the data of the next step in a sequence depends on the data of the previous step. 
#depend on the data of the previous step.

install.packages('keras')
install.packages('tidyverse')
install.packages('caret')
# einlesen der benötigten Packete
	
library(keras) # for deep learning
library(tensorflow)
#Falls es bei dem Model probleme gibt, muss die nächste Zeile hier ganz am Anfang ausgeführt werden
#install_tensorflow()
library(tidyverse) # general utility functions
library(caret) # machine learning utility functions
library(readr)
library(dplyr)
# Daten einlese _ read_csv2 verwendet ; als trennzeihen und das Komma als Dezimaltrennzeichen
mort_data <- read.csv("~/Desktop/WIRTSCHAFTSMATHEMATIK /WiMa 8. Semester/Mortality/Mort_Jahr.csv",dec=".",sep=";")

# check out the first few rows
head(mort_data)

#Erste spalte Jehr
#Zewite spalte Sterbefälle pro Jahr
#Dritte Spalte Sterbefälle pro Jahr pro 1000 Einwohner

# Paramter ----------------------------------------------------------------

#Parameter für das Model setzen
# Die maximale Läge einer Sequenz, welche betrachtet wird, um den nächsnte Punkt vorherzusage
#	batch size,Anzhal unterschiedlicher Sequenzen während des Trainings
#	die total anzahl an Epochen , die Anzahl die das Modell dem TRainigs ausgesetzt wird

# einige Parameter
max_len <- 6 # Die Anzhal vorheriger Punkte
batch_size <- 32 # anzhal der Sequenen für das Trainig 
total_epochs <- 15 # so oft wird der ganze Dtns
	
# set a random seed for reproducability
set.seed(123)

# daten vorbereiten -------------------------------------------------------

#nur die daten extrahieren, welche benötigt werden
mor<-mort_data$anteil
table(mor)
#The vector must now be divided into parts of the length max-lenght + 1, since from the previous vectors of the length max_lenght 
#der nachste Schritt vorhergesagt werden soll
#Dabei werden overlapping sequence gebildet
#in dem folgenden Beispiel ist die länge 3 und er overlap ist 2
#origonale  1 2 3 4 5 6 
#teil 1     1 2 3
#teil 2     2 3 4
#teil 3     3 4 5
#teil 4     4 5 6

# Startindizes für de Blocks
start_indexes <- seq(1, length(mor) - (max_len + 1), by = 3)

# Leere Matrix zum speichern
mor_matrix <- matrix(nrow = length(start_indexes), ncol = max_len + 1)

# Füllen der Matrix
for (i in 1:length(start_indexes)){
  mor_matrix[i,] <- mor[start_indexes[i]:(start_indexes[i] + max_len)]
}

#Voraussetzungen herstellen
#Sicherstellen dass die Daten numerische sind
#sicherstellen, dass es keine fehlenden werte gbit
	# Numerisch
	mor_matrix <- mor_matrix * 1
	
	# Fehlende werte entfernen
	if(anyNA(mor_matrix)){
	    mor_matrix <- na.omit(mor_matrix)
	}
	
	# Aufteilen in den Tag den wir vorhersagen wollen (y), und 
	# die sequenz welche dazu führt (X)
	X <- mor_matrix[,-ncol(mor_matrix)]
	y <- mor_matrix[,ncol(mor_matrix)]
#Aufteien in test und TRainingssaten
	# create an index to split our data into testing & training sets
	training_index <- createDataPartition(y, p = .9, 
	                                      list = FALSE, 
	                                      times = 1)
	
	# training data
	X_train <- array(X[training_index,], dim = c(length(training_index), max_len, 1))
	y_train <- y[training_index]
	
	# testing data
	X_test <- array(X[-training_index,], dim = c(length(y) - length(training_index), max_len, 1))
	y_test <- y[-training_index]

# Model festelegen --------------------------------------------------------
#mit Hilfe von Keras läßt sich das model einfach festelegen. die einzelnen Layer können in allegemeiner art und weise 
	#festegelegt werde
	
	# initializierung des modelles
	model <- keras_model_sequential()

	#ein Deep neural Network hat drei arten von Layern
x
#Hidden layer(s)
#The output layer

# input Layer -------------------------------------------------------------

	
	
# Input layer
	dim(X_train)
#	Die erste Dimensiton ist die Anzahl der trainigsbeispiele. Die zweite Dimension ist die Länge der Trainingssequenz. Die dritte Dimension ist die Anzahl der eigenschaften, welche untersucht werden soll.
#	Die erste Dimension kann vernachlässigt werden. Die Anzahl der Proben zu diesem Layer werden durch den Parameter Batch_size bestimmt. 
#Es wird die Dimension des Input Arrays genommen und als Input_shap an diesen Layer übergeben. Das Unit Argument gibt an wieviele Neuronen in diesem Layer verwendet werden sollen

	model %>%
	  layer_dense(input_shape = dim(X_train)[2:3], units = max_len)

# hidden layer ------------------------------------------------------------

# Desto mehr neurone in dem hidden Layer sind, desto eher kommt es zum Overfitting und desto länger dauert die Rechnung
	#Wenn die Anzhal der Neuronen zu gering gewählt wird, dann wird die rechnung nicht akkurat genug und die Gefahr des underfiting 
	#nimmt zu
	
	model %>% 
	  layer_simple_rnn(units = 6)
	

# Output Layer ------------------------------------------------------------
	#Die Aznahl der items.
	#hier 1
	model %>%
	  layer_dense(units = 1, activation = 'sigmoid') # output
	summary(model)

#das Model spezifizierne	
	
	model %>% compile(loss = 'loss_logcosh', 
	                  optimizer = 'RMSprop', 
	                  metrics = c('accuracy'))
	
#Das Model trainieren
	# das  kann etwas dauern
	trained_model <- model %>% fit(
	  x = X_train, 
	  y = y_train, 
	  batch_size = batch_size,
	  epochs = total_epochs,
	  validation_split = 0.1) 
	

	
	reticulate::py_last_error()

	

# Evaluation --------------------------------------------------------------

	trained_model
	
	plot(trained_model)
	
	
	classes <- model %>% predict_classes(X_test, batch_size = batch_size)
	
	# Confusion matrix
	table(y_test, classes)
	
	# baseline: just guess the weather will be the same as yesterday
	day_before <- X_test[,max_len - 1,1]
	
	# Confusion matrix
	table(y_test, day_before)
	
	# accuracy
	sum(day_before == classes)/length(classes)
	
	model %>% evaluate(X_test, y_test, batch_size = batch_size)
	      
	