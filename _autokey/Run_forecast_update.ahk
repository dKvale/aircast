#Persistent
SetTimer, aqi_update, 59500

return

aqi_update:
	
FormatTime, TimeToMeet,,HHmm


if (TimeToMeet = 0755)
{

        Run, "C:\Users\dkvale\Desktop\aircast\_batch\batch_R_aqi_forecast_update.bat"
	sleep, 60000

}



if (TimeToMeet = 1113)
{

        Run, "C:\Users\dkvale\Desktop\aircast\_batch\batch_R_aqi_forecast_update.bat"
	sleep, 60000

}



if (TimeToMeet = 1229)
{

        Run, "C:\Users\dkvale\Desktop\aircast\_batch\batch_R_aqi_forecast_update.bat"
	sleep, 60000

}



if (TimeToMeet = 1340)
{

        Run, "C:\Users\dkvale\Desktop\aircast\_batch\batch_R_aqi_forecast_update.bat"
	sleep, 60000

}



if (TimeToMeet = 1534)
{

        Run, "C:\Users\dkvale\Desktop\aircast\_batch\batch_R_aqi_forecast_update.bat"
	sleep, 60000

}

return

