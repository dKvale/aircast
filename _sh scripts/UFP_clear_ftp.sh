#!/bin/sh
ftp_path=/airvision
ftp_username=airvis
ftp_password=mpca
ftp_ip=34.216.174.58
ftp_port=21

for i in `curl -s -l ftp://"$ftp_username":"$ftp_password"@$ftp_ip/$ftp_path/ | grep 962_UFP20180123`; do
{
       echo "deleting ${ftp_path}/$i";
       curl ftp://${ftp_ip}:${ftp_port}/${ftp_path}/${i} -u "${ftp_username}:${ftp_password}" -O --quote "DELE ${ftp_path}/${i}"
};
done;
for i in `curl -s -l ftp://"$ftp_username":"$ftp_password"@$ftp_ip/$ftp_path/ | grep 962_UFP_2012_20171215`; do
{
  echo "deleting ${ftp_path}/$i";
  curl ftp://${ftp_ip}:${ftp_port}/${ftp_path}/${i} -u "${ftp_username}:${ftp_password}" -O --quote "DELE ${ftp_path}/${i}"
};
done;
for i in `curl -s -l ftp://"$ftp_username":"$ftp_password"@$ftp_ip/$ftp_path/ | grep 962_UFP_2012_20171218`; do
{
  echo "deleting ${ftp_path}/$i";
  curl ftp://${ftp_ip}:${ftp_port}/${ftp_path}/${i} -u "${ftp_username}:${ftp_password}" -O --quote "DELE ${ftp_path}/${i}"
};
done;
for i in `curl -s -l ftp://"$ftp_username":"$ftp_password"@$ftp_ip/$ftp_path/ | grep 962_UFP_2012_20180112`; do
{
  echo "deleting ${ftp_path}/$i";
  curl ftp://${ftp_ip}:${ftp_port}/${ftp_path}/${i} -u "${ftp_username}:${ftp_password}" -O --quote "DELE ${ftp_path}/${i}"
};
done;

 
