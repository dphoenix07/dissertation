PRO PLOT_RIP_ZTRAJ, run, state, $
	PLOT      = plot, $
	DIRECTORY = directory, $
	PNG		  = png, $
	EPS		  = eps, $
	TRAILTRAJ = trailtraj, $
	LEADTRAJ  = leadtraj, $
	CLOBBER   = clobber

;+
;NAME:
;     TRAJ3D_RAP_P
;PURPOSE:
;     This copies variables from ERA-Interim analysis into a single
;     file in pressure coordinates for use in TRAJ3D.
;     W at the surface is set to zero.  One pressure level is added at 
;     the top of the domain (p = 0), where w is also set to zero.
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     TRAJ3D_RAP_P, date0, outfile
;INPUT:
;		flight_name : RAF flight name (e.g., 'rf01')
;		direction   : 'forward' or 'backward'
;		ndays       : Length of trajectory run in days.  Default is 5.
;KEYWORDS:
;     PLOT      : If set, plot sample maps.
;     DIRECTORY : Output directory for wind file.
;	  CLOBBER   : If set, overwrite existing file. This is the default.
;OUTPUT:
;     Netcdf file.
;MODIFICATION HISTORY:
;		C. Homeyer:       2015-06-22.
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(run       ) EQ 0) THEN run        = '20120530_ncar'
IF (N_ELEMENTS(experiment) EQ 0) THEN experiment = 'd03_30km_icloud'
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1
IF (N_ELEMENTS(offset    ) EQ 0) THEN offset     = 0 
IF (N_ELEMENTS(offset    ) EQ 0) THEN offset     = 0 

ttraj1  = [9000.9 ,9091.4	,9120.9	,9112	,9083.6	,9108	,9107.3	,9065.4	,8973.3	,8994.7	,9091.6	,8993.1	,8953.7	,9019.5	,9143.1	,9302	,9272.8	,9396	,9535.1	, 9779	  ,9872.1	,9943.8		,10074.4	,10114.3	,10084.5	,10064		,10085.3	,10051.9 	,10073.4, 'NaN']	
ttraj2  = [9001	  ,8940.2	,9031.9	,9053.5	,8981.6	,8913.2	,8946.4	,8884.9	,8953.7	,8966.2	,9090.2	,9291	,9310.1	,9497.1	,9594.5	,9709.1	,9776.8	,9787.6	,9775.7	, 9746.1  ,9747.8	,9704.4		, 'NaN'		, 'NaN'		, 'NaN'		, 'NaN'		, 'NaN'		, 'NaN'		, 'NaN' , 'NaN']	
ttraj3  = [9000.8 ,9142.0	,9106.7	,9082	,9195.4	,9175.1	,9123.8	,9129.1	,9040	,9081.3	,9070.3	,9043.6	,9298.8	,9393.8	,9526.2	,9544.7	,9906.7	,10035.8,10176.3, 10176.3 ,10090.3	,10024.9	, 'NaN'		, 'NaN'		, 'NaN'		, 'NaN'		, 'NaN'		, 'NaN'		, 'NaN'	, 'NaN']	
ttraj4  = [9000.9 ,8974.4	,8926.5	,9011.2	,9101.7	,9045.5	,8984.8	,8953.8	,8936.8	,8818.2	,8743.6	,8788.5	,8791.3	,8953.8	,8886.9	,8907.4	,8978.7	,9100.5	,9068.8	, 9260	  ,9471.5	,9574		,9702.3		,9753.1		,9892.2		,9992.1		,10186.7	,10222		,10169	,10075.2]
ttraj5  = [9001	  ,9075.8	,9105.7	,9076.4	,9075.6	,9051.4	,9017.6	,9033	,9080.4	,8922.6	,9075.9	,9036.8	,8979.5	,8921.7	,9162.7	,9330.8	,9287	,9422.1	,9647.5	, 9885.6,	10011.6	,10059.6	,10124.2	,10104.4	,10076.8	,10052.4	,10018.2	, 'NaN'		, 'NaN'	, 'NaN']
ttraj6  = [9001	  ,9049.7	,9017.8	,9026.6	,9081	,9113.2	,9057.5	,8985.9	,9010.1	,9054.1	,8858.2	,9095.8	,9223.1	,9313.9	,9327.6	,9682.8	,9800.9	,9930.4	,10031.3, 9993.2,	9963.5	,9964.6		,9912.6		, 'NaN'		, 'NaN'		, 'NaN'		, 'NaN'		, 'NaN'		, 'NaN'	, 'NaN']	
ttraj7  = [9001	  ,9032.5	,9067.3	,9053.6	,9030	,9017.1	,9078	,9066.1	,8922.8	,9046.5	,9017.4	,8865.9	,8967.6	,9225	,9246.2	,9312.8	,9466.3	,9846.4	,9913.8	, 10000.2,	10045.1	,10005.1	,9981.2		,9968.2		,9926		, 'NaN'		, 'NaN'		, 'NaN'		, 'NaN'	, 'NaN']	
ttraj8  = [9001	  ,8943.2	,9105.4	,9151.3	,9119.5	,9082.1	,9046.4	,9051.1	,8884.7	,9006.1	,9012.3	,8891.8	,9096.3	,8888.9	,8973.4	,9133.6	,9218.3	,9334.6	,9443.6	, 9597.3,	9785.3	,9987.3		,10050.8	,10161.4	,10190.1	,10172.4	,10094.4	,10094.7	, 'NaN'	, 'NaN']
ttraj9  = [9001	  ,9057.8	,9139.1	,9152	,9103.4	,9118.5	,9087.4	,9110.8	,9097.4	,9078.5	,8962.5	,9110.1	,8994	,8998.4	,9200.1	,9392.7	,9391	,9489	,9753.7	, 10074.5,	10125.3	,10209.7	,10215.1	,10198.3	,10127.8	,10124.2	, 'NaN'		, 'NaN'		, 'NaN'	, 'NaN']
ttraj10 = [9001	  ,9108.7	,9141.8	,9046	,9106.5	,9154.2	,9123.1	,9111.1	,9030.3	,9008.1	,9071.7	,8927	,9030.5	,9336.6	,9326.3	,9423.6	,9606	,9961.6	,10006.3, 10115.6,	10153.7	,10108		,10030.4	,10058		, 'NaN'		, 'NaN'		, 'NaN'		, 'NaN'		, 'NaN'	, 'NaN']	
ttraj11 = [9001	  ,9078.8	,9207.5	,9190.4	,9208.2	,9131.7	,9078.6	,9073.6	,9029.6	,9095.1	,8973.6	,9102.9	,9077.2	,8966.2	,9087.5	,9211.6	,9379.3	,9387.1	,9551.4	, 9722.8,	9941.3	,10086.9	,10133.3	,10217.2	,10215.2	,10197.3	,10103.9	, 'NaN'		, 'NaN'	, 'NaN']
ttraj12 = [9001	  ,9106.4	,9149.1	,9171.9	,9117.6	,9089.4	,9108.5	,9153.8	,9117.4	,9024.3	,9126.6	,9050.4	,8971	,9130.2	,9359.4	,9392.9	,9446.3	,9589.4	,10011	, 10091.5,	10180.1	,10170.1	,10179.9	,10155.8	,10076.2	, 'NaN'		, 'NaN'		, 'NaN'		, 'NaN'	, 'NaN']	
ttraj13 = [9001	  ,9052.0	,9051.6	,8998.8	,9113.9	,9111.6	,9062.9	,9104.9	,8912.9	,9088.6	,8905.8	,8950.5	,9189.1	,9275.6	,9368	,9418.7	,9821.2	,9880.3	,10047.8, 10009.1,	10028.5	,9996.2		,9929.6		, 'NaN'		, 'NaN'		, 'NaN'		, 'NaN'		, 'NaN'		, 'NaN'	, 'NaN']	
ttraj14 = [9001	  ,8884.7	,8967.4	,9089.1	,9040.9	,8998.9	,9008.6	,8995.4	,8843.4	,8971.5	,9004.8	,8889.5	,9045.6	,8920.3	,8955.6	,9114.4	,9166.8	,9316.6	,9414.7	, 9565	,	9744	,9947.4		,10049.4	,10140.4	,10201.4	,10186.9	,10117.9	,10093.9	, 'NaN'	, 'NaN']
ttraj15 = [9001	  ,8971.3	,9113.5	,9118.7	,9084.9	,9034.1	,9083.8	,9063.3	,9074	,9092.5	,8929.6	,9121.7	,8950.1	,8965.6	,9140.6	,9337.4	,9381.8	,9476.4	,9647.3	, 10026.9,	10129.9	,10194.1	,10225.1	,10209.3	,10145.2	,10120.6	, 'NaN'		, 'NaN'		, 'NaN'	, 'NaN']

ltraj1  = [10472	,10473.7	,10463.1	,10467.3	,10468.5	,10443.4	,10443.1	,10446.7	,10423.6	,10403.1	,10390.8	,10367.1	,10333.6	,10302.7	,10294.7	,10275.1	,10237.6	,10211.7	,10191.1	,10165.4	,10138		,10115.1	,10111.5	,10106.8	,10074.7	,10049.6  ,10032.7	,10011.1	,9993.5	,9965.9	,9933.1	,9906.7	,9873.3	,9840.6	,9816.9	,9787.6	,9755.9	,9723.7	,9683.3	,9645.4	,9619.3	,9590.2	,9556.7	,9523.5	,9490	,9464.8	,9441	,9397.5	,9337.1	,9277.2	,9238.3	,9194.7	,9145	,9068.3	,9001.1]
ltraj2  = [10477.7	,10493.3	,10485.1	,10470.5	,10478		,10468.4	,10452.3	,10439.9	,10425.5	,10409.8	,10389.8	,10363	    ,10339.5	,10322.5	,10302		,10272.8	,10246.7	,10226.6	,10204.1	,10184.5	,10168.3	,10155.4	,10139.7	,10121.5	,10102.4	,10081.3  ,10066.6	,10047.9	,10017.9,9983.5	,9953.6	,9921.6	,9887.9	,9855.6	,9820.8	,9787.6	,9761.3	,9733.8	,9702.2	,9676.8	,9643.9	,9607.8	,9573.2	,9536.1	,9508.7	,9484.8	,9448.4	,9400.4	,9336	,9272.3	,9233.8	,9190.1	,9112.1	,9063.6	,9001.1]
ltraj3  = [10487	,10481.3	,10477.2	,10476.9	,10467.5	,10456.8	,10449.1	,10430.2	,10413.6	,10396.3	,10378.1	,10352	    ,10335.6	,10320.2	,10290.9	,10264.5	,10243.5	,10225.4	,10207		,10188.8	,10171.1	,10153.5	,10138.9	,10128.5	,10107.3	,10084.1  ,10066.7	,10042.9	,10010.6,9980.5	,9945.9	,9920.1	,9893.8	,9846.1	,9801.1	,9780.6	,9752.4	,9722.7	,9705.6	,9682.3	,9647.9	,9613.9	,9582.5	,9551.8	,9511.7	,9474.4	,9428	,9374.2	,9298.5	,9246.1	,9206.3	,9131.5	,9105.2	,9056.3	,9001]
ltraj4  = [10414.3	,10401.2	,10405.6	,10403		,10393.8	,10388.6	,10368.3	,10353		,10336.2	,10316		,10300.2	,10289.2	,10270.7	,10239		,10211.6	,10192.3	,10170.3	,10149.9	,10130.5	,10109.8	,10088		,10069.3	,10055.4	,10036.5	,10016.1	,9993.7	  ,9969.2	,9942.9	    ,9913.4	,9878.8	,9853.6	,9833.4	,9787.3	,9741	,9710.4	,9674.3	,9638.5	,9624.7	,9608.6	,9579.9	,9557.6	,9532.7	,9495.4	,9455.7	,9413.4	,9373.6	,9320.9	,9257.3	,9204.2	,9157.9	,9110.3	,9058.8	,9044.9	,9006.2	,9001]
ltraj5  = [10285.3	,10290.6	,10295.8	,10275.3	,10253.8	,10282.1	,10312.7	,10301.4	,10282.3	,10283.2	,10265.6	,10247.6	,10247.2	,10231.3	,10193		,10177.9	,10164.1	,10132.5	,10108.4	,10079.5	,10024.8	,9998.7	    ,9987.2		,9956.2		,9940	    ,9912.3	  ,9856.7	,9841	    ,9823.6	,9790	,9781.1	,9764.9	,9728.2	,9699.8	,9665.2	,9638.6	,9629.9	,9601.4	,9569	,9537.5	,9486.3	,9442.8	,9401.4	,9360.9	,9342.9	,9325.6	,9301.6	,9289.4	,9240.9	,9205.2	,9174.5	,9137.6	,9089.5	,9040.5	,9001.1]
ltraj6  = [10196.4	,10190.8	,10194.6	,10187.5	,10206.3	,10182		,10179.8	,10169		,10146.2	,10176.8	,10151.3	,10129.2	,10126.2	,10100.5	,10103.1	,10085.8	,10040.9	,10026.5	,10028.8	,9999.9		,9958.4		,9930.2	    ,9883.4		,9861.6		,9851.8	    ,9821.2	  ,9795.9	,9773.1	    ,9753.2	,9727.2	,9709.6	,9681.3	,9632.7	,9601.1	,9573.2	,9565.1	,9566.4	,9544.2	,9527.2	,9477.4	,9419.9	,9390.4	,9339.7	,9317	,9276.4	,9243.6	,9247.5	,9215.5	,9196.6	,9169.1	,9127.8	,9100.6	,9069.4	,9025.9	,9001.1]
ltraj7  = [10192.1	,10192.1	,10178		,10175.6	,10178.8	,10156.5	,10139.5	,10178.7	,10151.3	,10139.1	,10161.9	,10142.4	,10117.5	,10111.1	,10100.3	,10080.6	,10041.2	,10013.3	,9999		,9978.4		,9956.4		,9913.2	    ,9891.3		,9867		,9840.4	    ,9816.5	  ,9775.2	,9758.7	    ,9746.4	,9719.9	,9692.8	,9666.7	,9630.4	,9608.6	,9574.7	,9547.3	,9530.1	,9501.5	,9467.6	,9436.4	,9415.9	,9395.4	,9373.3	,9319.4	,9269.2	,9221.6	,9205.1	,9183.2	,9181.2	,9158.4	,9116.2	,9080.5	,9067.9	,9028.7	,9001.1]
ltraj8  = [10694.7	,10668.5	,10685.6	,10688.9	,10688.5	,10680.1	,10652		,10648.7	,10645.1	,10628.1	,10603.6	,10590.7	,10565.3	,10543.8	,10518.1	,10500.5	,10493.6	,10479.6	,10470.9	,10465.7	,10444.1	,10429.3	,10422.4	,10381.6	,10341.5	,10312.6  ,10279.6	,10246.2	,10217.8,10182.3,10133.6,10089.1,10067.8,10041.8,10007.3,9973.9	,9941.7	,9903.9	,9858.8	,9816.9	,9781.6	,9742.2	,9687.3	,9632.5	,9593.3	,9553.8	,9497.5	,9434.5	,9380.7	,9324.4	,9279.3	,9251.3	,9196.5	,9106.7	,9001.1]
ltraj9  = ['NaN'	,'NaN'		,'NaN'		,'NaN'		,'NaN'		,'NaN'		,'NaN'		,10477.5	,9899.6		,9896.5		,9686.3		,9770		,10070.9	,10301.9	,10401.1	,10315.2	,10248.7	,10237.3	,10222.3	,10189.6	,10149		,10155.8	,10129.9	,10111.9	,10118.9	,10098.2  ,10070.6	,10049.3	,10031.4,10008.2,9976	,9952.2	,9918.1	,9890	,9868.8	,9845.3	,9826	,9803.2	,9781.3	,9749.6	,9706	,9654.8	,9602.4	,9547.6	,9506	,9468.3	,9430.5	,9395.3	,9363.9	,9334.4	,9295.6	,9254.3	,9192.6	,9099.9	,9001.1]
ltraj10 = ['NaN'	,'NaN'		,'NaN'		,'NaN'		,'NaN'		,9786.6		,9880.3		,9956.1		,10130.7	,10282.8	,10346.7	,10338.9	,10279.7	,10252.5	,10128.4	,10063.6	,9984.2		,9941.8		,9887.9		,9848.3		,9884.8		,9887.6		,9885.5		,9892.9		,9893.4		,9885.9	  ,9877.9	,9866.7	    ,9854.7	,9833.9	,9811.7	,9784.7	,9749.7	,9729.9	,9712.1	,9698.5	,9677.2	,9641.1	,9600.6	,9569.1	,9537.2	,9505.2	,9483.5	,9462.6	,9434.5	,9405.3	,9373.7	,9348.1	,9319.3	,9295.9	,9264.1	,9207.5	,9144.9	,9074.8	,9001.1]
ltraj11 = [10446.6	,10430		,10430.7	,10449.2	,10442.3	,10410		,10416.6	,10420.7	,10393.8	,10384.1	,10366.1	,10337.6	,10325.8	,10288		,10256.7	,10240.6	,10212.8	,10189.6	,10163.6	,10120.3	,10104.1	,10092.4	,10059.4	,10028.4	,10011.3	,9996.2	  ,9964.7	,9940.6	    ,9938.3	,9922.6	,9888.1	,9856.1	,9827	,9800.5	,9777.6	,9754.6	,9725.6	,9694	,9645.9	,9592.6	,9553.9	,9515.5	,9482.9	,9460.8	,9442	,9424.8	,9395.7	,9364.4	,9323.1	,9264.4	,9229	,9192.6	,9146.6	,9086.7	,9001.1]
ltraj12 = [10414.8	,10423.6	,10426.3	,10392.5	,10369.6	,10403.6	,10424.8	,10393.6	,10372		,10388.5	,10364.1	,10326.7	,10316.6	,10297.3	,10262.3	,10228.6	,10191.6	,10168.6	,10149.8	,10113.4	,10070		,10048.3	,10030.6	,9997.3		,9973.4	    ,9957.8	  ,9919.4	,9884.4	    ,9876.7	,9856.3	,9835.8	,9828	,9804.6	,9775	,9752.4	,9728.4	,9697.7	,9672.8	,9638.7	,9588.5	,9542.5	,9500	,9463.2	,9439.4	,9424.8	,9402.2	,9376.7	,9345.6	,9300.4	,9245.1	,9218	,9183	,9136.6	,9082.4	,9001.1]
ltraj13 = [10402.6	,10395		,10406		,10411.1	,10387.8	,10354.4	,10355.4	,10391.9	,10392.8	,10365.3	,10352.9	,10347.2	,10314		,10287.5	,10277.8	,10247.2	,10202.7	,10178.5	,10148.7	,10110.1	,10071.4	,10042.1	,10007.3	,9970		,9949	    ,9935.2	  ,9911.9	,9889.6	    ,9861.5	,9822.9	,9802.2	,9786.3	,9762.9	,9749.9	,9737.5	,9720.7	,9701.4	,9664.1	,9611.6	,9562.4	,9509.2	,9453.6	,9431	,9420.8	,9400.2	,9393.5	,9382.6	,9341.6	,9296.7	,9253	,9219.7	,9183.5	,9156.6	,9090.5	,9001.1]
ltraj14 = ['NaN'	,'NaN'		,'NaN'		,'NaN'		,10643.4	,10390.2	,10445.2	,10448.9	,10168.2	,9887		,9851.8		,9769.2		,9937.2		,10009.6	,10020		,10100.6	,10130.2	,10074.7	,10052.5	,10040.1	,10045.8	,10035.9	,10019.6	,9972		,9969.3	    ,9929.2	  ,9899.4	,9871.7	    ,9847	,9830.3	,9804.6	,9775.3	,9746.9	,9729.7	,9707.8	,9681.8	,9653.3	,9616.4	,9576.2	,9537.4	,9494.7	,9449.1	,9411.1	,9379.4	,9353.7	,9342.5	,9342.8	,9357.8	,9358.3	,9319.6	,9233.4	,9123.8	,9028.2	,8981.7	,9001.1]
ltraj15 = [10196.7	,10190.3	,10197.4	,10194.5	,10176.3	,10182.4	,10147.4	,10178.1	,10169.8	,10147.1	,10144.6	,10120.1	,10124.5	,10115.4	,10075.3	,10062.3	,10063.3	,10047.4	,10021.9	,9983.3		,9965		,9944.8		,9925.6		,9902.8		,9869.9	    ,9853.6	  ,9825.2	,9795.4	    ,9764.4	,9730	,9720.8	,9708.6	,9692.6	,9644.9	,9605.6	,9588.3	,9544.1	,9533.9	,9520.3	,9484.5	,9442.6	,9391	,9358	,9326.2	,9302	,9284.7	,9246.7	,9225.8	,9205.3	,9174.7	,9131.4	,9085.8	,9055.4	,9031.9	,9001.1]

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/rip_traj_ztime/'

IF (KEYWORD_SET(trailtraj)) THEN BEGIN
	epsfile = outdir + 'trailtraj.eps'						        ;EPS filename
	pdffile = outdir + 'trailtraj.pdf'						        ;PDF filename
	pngfile = outdir + 'trailtraj.png'						        ;PNG filename
ENDIF

IF (KEYWORD_SET(leadtraj)) THEN BEGIN
	epsfile = outdir + 'leadtraj.eps'						        ;EPS filename
	pdffile = outdir + 'leadtraj.pdf'						        ;PDF filename
	pngfile = outdir + 'leadtraj.png'						        ;PNG filename
ENDIF

FILE_MKDIR, outdir																								;Create output directory, if necessary
    
IF KEYWORD_SET(z_buff) THEN BEGIN
	SET_PLOT, 'Z'																									;Output to Z buffer
	DEVICE, SET_PIXEL_DEPTH = 24, SET_RESOLUTION = [wfactor*(dim[0]), wfactor*(dim[1])], $	;Set device resolution and bit depth
		SET_CHARACTER_SIZE = [12, 20]
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 1.5																							;Set character size
	!P.FONT       = -1
ENDIF ELSE BEGIN
	IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN	
		PS_ON, FILENAME = epsfile, PAGE_SIZE = [8.0,4.0], MARGIN = 0.0, /INCHES;PAGE_SIZE = 0.001*dim*wfactor			;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.FONT     = 0																								;Hardware fonts
		!P.CHARSIZE = 0.75	
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS																							;Load basic color definitions
	ENDIF ELSE BEGIN
		SET_PLOT, 'X'
		WINDOW, XSIZE = wfactor*(dim[0]), YSIZE = wfactor*(dim[1])										;Open graphics window
		!P.COLOR      = COLOR_24('black')																		;Foreground color
		!P.BACKGROUND = COLOR_24('white')																		;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1																							;Use Hershey fonts
	ENDELSE
ENDELSE

STOP
PLOT, REVERSE(ttraj1), /NODATA, $
	TITLE    = 'Backward Trajectories from Trailing Anvil', $
	XTITLE   = 'Time (UTC)', $
	YTITLE   = 'Trajectory Height (km)', $
	YRANGE   = [8.0, 11.0], $
	CHARSIZE = 2

OPLOT, REVERSE(ttraj1 )*1.0E-3, THICK = 3
OPLOT, REVERSE(ttraj2 )*1.0E-3, THICK = 3
OPLOT, REVERSE(ttraj3 )*1.0E-3, THICK = 3
OPLOT, REVERSE(ttraj4 )*1.0E-3, THICK = 3
OPLOT, REVERSE(ttraj5 )*1.0E-3, THICK = 3
OPLOT, REVERSE(ttraj6 )*1.0E-3, THICK = 3
OPLOT, REVERSE(ttraj7 )*1.0E-3, THICK = 3
OPLOT, REVERSE(ttraj8 )*1.0E-3, THICK = 3
OPLOT, REVERSE(ttraj9 )*1.0E-3, THICK = 3
OPLOT, REVERSE(ttraj10)*1.0E-3, THICK = 3
OPLOT, REVERSE(ttraj11)*1.0E-3, THICK = 3
OPLOT, REVERSE(ttraj12)*1.0E-3, THICK = 3
OPLOT, REVERSE(ttraj13)*1.0E-3, THICK = 3
OPLOT, REVERSE(ttraj14)*1.0E-3, THICK = 3
OPLOT, REVERSE(ttraj15)*1.0E-3, THICK = 3

PLOT, ltraj1, /NODATA, $
	TITLE    = 'Backward Trajectories from Leading Anvil', $
	XTITLE   = 'Time (UTC)', $
	YTITLE   = 'Trajectory Height (km)', $
	YRANGE   = [8.0, 11.0], $
	CHARSIZE = 2

OPLOT, (ltraj1 )*1.0E-3, THICK = 3
OPLOT, (ltraj2 )*1.0E-3, THICK = 3
OPLOT, (ltraj3 )*1.0E-3, THICK = 3
OPLOT, (ltraj4 )*1.0E-3, THICK = 3
OPLOT, (ltraj5 )*1.0E-3, THICK = 3
OPLOT, (ltraj6 )*1.0E-3, THICK = 3
OPLOT, (ltraj7 )*1.0E-3, THICK = 3
OPLOT, (ltraj8 )*1.0E-3, THICK = 3
OPLOT, (ltraj9 )*1.0E-3, THICK = 3
OPLOT, (ltraj10)*1.0E-3, THICK = 3
OPLOT, (ltraj11)*1.0E-3, THICK = 3
OPLOT, (ltraj12)*1.0E-3, THICK = 3
OPLOT, (ltraj13)*1.0E-3, THICK = 3
OPLOT, (ltraj14)*1.0E-3, THICK = 3
OPLOT, (ltraj15)*1.0E-3, THICK = 3
    
IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file
  

END
