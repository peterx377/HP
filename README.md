To simulate daily counts of covid-19 infection in each state with Hawkes Process, we construct three steps to accommodate to the lengthy runtime of the simulation.Each step is completed in its respective folder in HP. Also, we have a correspoonding folder with the same name in the data folder for data storage.

I:
  - create_data_new_format.R
    - retrieve original covid-19 daily counts of each state
    - store the data in state_covid_confirmed.csv in its twin folder in data
  - dataCleanup.R
    - retrieve original data and rescale it by corresponding rescale factor for each state
    - Calculate 7-day moving average daily counts for each state
    - Store the processed data in state_covid_confirmed-7ma.csv 
  - EM_covid.m 
    - This matlab file utilized param_eval.m to learn parameters needed for the simulation of Hawkes Process
    - The learned parameters are stored in /data/I/param
  - Simulation.R
    - Use Hawkes Process to simulate covid-19 daily cases in each state
    - data is stored in /data/I/state

II:
  - updateData.R
    - retrieve new data since I
  - dataCleanup
    - calculate 7-day moving average of covid-19 daily cases
  - updateParam.R
    - use learn_update.R to calculate the parameters for Hawkes Process with new data feed
    - store new parameters in data/II/param

III:
  - updateData
    - retrieve new data since II
  - dataCleanup.R
    - calculate 7-day moving average 
  - Simulation.R
    - use parameters learned in II and duplicate the last reproduction number in rList by the number of new days/10

webapp:
  - ShinyApp displays the trend of pandemic in each state in different time frames
