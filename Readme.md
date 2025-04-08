#### Modelling of electrical machine using Time Domain Multiconductor Transmission Line (MTL) Theory
*The purpose of the model is to understand the high-frequency transients due to switching pulses of frequency drive systems.*

I understand this repository requires a wiki to describe theories supporting the code. I'll complete it during my spare time. Feedback appreciated!
* Input files to this Mathematica model result from Finite Element Analysis.
* Cedrat Flux has been used for Finite Element Analysis of MTL parameters, i.e. inductances and capacitances.
* Capacitances of MTL conductors are calculated using the electrostatic solver, and the magnetostatic solver is used to calculate inductance.
* Python scripts run the solvers for each conductor geometric position and save parameters in Excel files.
* CSV files are the experimental measurements to verify the model.
* These Excel files are imported to Wolfram Mathematica for MTL modelling as illustrated below:

![MTL regions](img/f3-5.jpg)
**Fig. 1** 
*Definition of different zones of electrical machine for parametrising and modelling transients with MTL*

#
![MTL equations](img/f3-6a.jpg)
**Fig. 2** 
*MTL equations illustration*


* This repository contains Wolfram Mathematica code in three forms:
  1. Notebook file *MTL-131195-n=68.nb* if you access the Mathematica front-end.
  2. Wolfram script package file *MTL-131195-n=68.wl* can be used with the Wolfram Script free licence.
  3. The following images with the output waveforms are shown for only reading the code.


![MTL-131195-n=68_1.gif](img/MTL-131195-n=68_1.gif)

![MTL-131195-n=68_6.gif](img/MTL-131195-n=68_6.gif)

![MTL-131195-n=68_8.gif](img/MTL-131195-n=68_8.gif)

![MTL-131195-n=68_9.png](img/MTL-131195-n=68_9.png)

![MTL-131195-n=68_10.png](img/MTL-131195-n=68_10.png)

![MTL-131195-n=68_11.png](img/MTL-131195-n=68_11.png)

![MTL-131195-n=68_12.gif](img/MTL-131195-n=68_12.gif)

![MTL-131195-n=68_13.png](img/MTL-131195-n=68_13.png)

![MTL-131195-n=68_14.gif](img/MTL-131195-n=68_14.gif)

![MTL-131195-n=68_15.gif](img/MTL-131195-n=68_15.gif)

![MTL-131195-n=68_16.png](img/MTL-131195-n=68_16.png)

![MTL-131195-n=68_17.gif](img/MTL-131195-n=68_17.gif)

![MTL-131195-n=68_18.png](img/MTL-131195-n=68_18.png)

![MTL-131195-n=68_19.gif](img/MTL-131195-n=68_19.gif)

![MTL-131195-n=68_20.png](img/MTL-131195-n=68_20.png)

![MTL-131195-n=68_21.gif](img/MTL-131195-n=68_21.gif)

![MTL-131195-n=68_22.png](img/MTL-131195-n=68_22.png)

![MTL-131195-n=68_23.gif](img/MTL-131195-n=68_23.gif)

![MTL-131195-n=68_24.gif](img/MTL-131195-n=68_24.gif)

![MTL-131195-n=68_25.png](img/MTL-131195-n=68_25.png)

![MTL-131195-n=68_26.gif](img/MTL-131195-n=68_26.gif)

![MTL-131195-n=68_27.png](img/MTL-131195-n=68_27.png)

![MTL-131195-n=68_28.gif](img/MTL-131195-n=68_28.gif)

![MTL-131195-n=68_29.gif](img/MTL-131195-n=68_29.gif)

![MTL-131195-n=68_30.png](img/MTL-131195-n=68_30.png)

![MTL-131195-n=68_31.gif](img/MTL-131195-n=68_31.gif)

[Created with the Wolfram Language](http://www.wolfram.com/language/)