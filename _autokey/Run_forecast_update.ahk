#Persistent
SetTimer, aqi_update, 59500

return

aqi_update:
	
FormatTime, TimeToMeet,,HHmm


if (TimeToMeet = 0735)
{

        Run, "C:\Users\dkvale\Desktop\hysplit\BATCH_R_forecast_Update.bat"
	sleep, 60000

}



if (TimeToMeet = 1112)
{

        Run, "C:\Users\dkvale\Desktop\hysplit\BATCH_R_forecast_Update.bat"
	sleep, 60000

}



if (TimeToMeet = 1225)
{

        Run, "C:\Users\dkvale\Desktop\hysplit\BATCH_R_forecast_Update.bat"
	sleep, 60000

}



if (TimeToMeet = 1337)
{

        Run, "C:\Users\dkvale\Desktop\hysplit\BATCH_R_forecast_Update.bat"
	sleep, 60000

}



if (TimeToMeet = 1534)
{

        Run, "C:\Users\dkvale\Desktop\hysplit\BATCH_R_forecast_Update.bat"
	sleep, 60000

}

return

