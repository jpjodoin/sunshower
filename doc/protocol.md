Decoding
============
Decoding is a variant of base64. Only difference is that char 62 is a - instead of a + like in base64url
~~~
A-Z [0-25]
a-z [26-51]
0-9 [52-61]
- 62
/ 63
~~~


Establishing connection
============

Client -> Server
~~~
GET /app/3fc4c501186e141227fb?client=melnor&version=1.0&protocol=6 HTTP/1.1
Host: ws.pusherapp.com:80
Upgrade: WebSocket
Connection: Upgrade
Sec-WebSocket-Key: x3JJHMbDL1EzLkh9GBhXDw==
Origin: MelnorSprinklerSystem
Sec-WebSocket-Version: 13
~~~

Server -> Client
~~~
HTTP/1.1 101 Switching Protocols
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Accept: HSmrc0sMlYUkAGmm5OPpG2HaGWk=
~~~
Server -> Client
~~~
{"event":"pusher:connection_established","data":"{\"socket_id\":\"241972.12910432\"}"}
~~~
Client -> Server
~~~
{"data": {"channel": "<RfSerial>"}, "event": "pusher:subscribe"}
~~~
Server -> Client 
~~~
{"event":"pusher_internal:subscription_succeeded","data":"{}","channel":"<RfSerial>"}
~~~


Initialization request
============


Getting Hash
------------
The hash will be used in all future request to identify the RainCloud client system.

Client -> Server
~~~
GET /submit/?idhash=0000000000&message=1xOuOYDYAQCgAi3dAP8AAgAAAAAAAg== HTTP/1.1
Host: wifiaquatimer.com
Connection: close

HTTP/1.0 200 OK
~~~

Server -> Client (over websocket)
~~~
{"event":"hash_key","data":"\"5b433cbc37\"","channel":"d88039ae13d7"}
~~~

~~~
GET /submit/?idhash=5b433cbc37&message=ascii--hashkeyevnt--ack--null HTTP/1.1
Host: wifiaquatimer.com
Connection: close

HTTP/1.0 200 OK
~~~

Fetching schedule
------------
This request is repeated for schedule 0 to 6

Server -> Client (over websocket)
~~~
{"event":"sched_day0","data":"\"Ld0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=\"","channel":"d88039ae13d7"}
~~~

Client -> Server
~~~
GET /submit/?idhash=5b433cbc37&message=ascii--Day0scheduleevnt--ack--null HTTP/1.1
Host: wifiaquatimer.com
Connection: close

HTTP/1.0 200 OK
~~~

Manual schedule???
------------

Server -> Client (over websocket)
~~~
{"event":"manual_sched","data":"\"Ld0AAAAAAAAAAAAAAAAAAAAA\"","channel":"d88039ae13d7"}
~~~
Client -> Server
~~~
GET /submit/?idhash=5b433cbc37&message=ascii--manualctrlevnt--ack--null HTTP/1.1
Host: wifiaquatimer.com
Connection: close

HTTP/1.0 200 OK
~~~

Sync timestamp
------------
Server -> Client (over websocket)
~~~
{"event":"timestamp","data":"\"zwED\"","channel":"d88039ae13d7"}
~~~
base64 decode to hex
zwED -> cf 01 03

01 cf  -> 463 minutes since the start of day 




Software version????
------------
Server -> Client (over websocket)
~~~
{"event":"rev_request","data":"\"\"","channel":"d88039ae13d7"}
~~~
Client -> Server
~~~
GET /submit/?idhash=5b433cbc37&message=ascii--revisions--U501--0010--75-FF HTTP/1.1
Host: wifiaquatimer.com
Connection: close
~~~


Periodic status update
============
Every minutes, the client will send a request of the type

~~~
GET /submit/?idhash=5b433cbc37&message=1xOuOYDYAwDQAS3dAP8AAgAAAAAAAg== HTTP/1.1
Host: wifiaquatimer.com
Connection: close
~~~

Server will send a websocket keep alive every 200 seconds



Message format
=============
~~~
1xOuOYDYAwDQAS3dAP8AAgAAAAAAAg==
~~~
base64url decode
~~~
d7 13 ae 39 80 d8 03 00 d0 01 2d dd 00 ff 00 02 00 00 00 00 00 02
[---<RfSerial>--][DW][?][-MM-][VaI][BB][BA][ST]<-Humidity Sensor-> 

DW: Day of week (0 sunday, sathurday 6) 
VaI: Valve Id (ex: DD2D under the Valve)
MM: minutes since the start of the day ex: d0 01 -> 01 d0 -> 464 7h44
BB: binary mask (0x1 button 1, 0x2 button 2, 0x4 button 3, 0x8 button 4) (0x11 web valve 1, 0x22 web valve 2, 0x44 web valve 3, 0x88 valve 4)
BA: Battery FF = 99%, FE = 98% FD=97%  FC = 95% F7 = 88%   F2= 80% ED = 73%


18
ST: Status 00: Connecté 01: Valve non-connecté... 02: Déconnecté depuis + de 5 minutes
~~~



Websocket message
=============
~~~
{"event":"manual_sched","data":"\"Ld0AAAAAmgWaBQAAAAAAAAAA\"","channel":"d88039ae13d7"}
2d dd 00 00 00 00 9a 05 9a 05 00 00 00 00 00 00 00 00
[VaId][-V1-][-V2-][-V3-][-V4-][---Multiple valve??---] 

VaId: Valve Id (ex: DD2D under the Valve)
V#: 00 00 valve is closed
    ^-Number of minutes where it should stop


{"event":"manual_sched","data":"\"Ld2fBQAAAAAAAAAAAAAAAAAA\"","channel":"d88039ae13d7"}

2d dd 9f 05 00 00 00 00 00 00 00 00 00 00 00 00 00 00  Cap à 23h59, so valeur max = 9f05
     
~~~



%1xOuOYDYAwDQAS3dAAUDANABLd0AAA==  -> actiavte humidity sensor