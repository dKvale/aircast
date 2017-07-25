#Persistent
SetTimer, aqi_update, 59500

return

aqi_update:
	
FormatTime, TimeToMeet,,HHmm


if (TimeToMeet = 0630)
{

        Run, "C:\Users\dkvale\Desktop\hysplit\BATCH_R_forecast_Update.bat"
	sleep, 60000

}



if (TimeToMeet = 1105)
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

return

