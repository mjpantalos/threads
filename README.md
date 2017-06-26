# threads
Determine the change in force at each individual thread break

The master_threads file has all the code that we want to run. I will also include some reference files for work that I have done previously.

The sample specimen data file is included in this folder. 

Possible peak finding function code is also included here. I've included two different methods that both work similarly well in finding peaks. Perhaps running these after a smoothing function, etc., or just finding the delta = y(peak) - y(pit) would be fine as is, and then ignoring deltas that are approx. 0. 

The file Su2016_thread_peaks.R is code from summer 2016, where I worked with a sample file in finding the deltas. The issue I ran into was that the code needs to be groundtruthed. 
