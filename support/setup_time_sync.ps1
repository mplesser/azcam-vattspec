reg add HKLM\SYSTEM\CurrentControlSet\Services\W32Time\TimeProviders\NtpClient /v SpecialPollInterval /t reg_dword /d 120 /f

w32tm /config /manualpeerlist:"10.0.3.17 128.196.248.150 0.pool.ntp.org tick.usno.navy.mil time-b.nist.govtime-b.nist.gov" /syncfromflags:MANUAL /reliable:NO /update

net stop w32time
net start w32time
w32tm /resync /rediscover
