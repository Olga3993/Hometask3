echo off
echo Edit this file to set proper erl.exe path!
echo Navigate your browser to http://localhost:8090/
echo on

"C:\Program Files\erl6.4\bin\erl.exe" -noshell -pa ./ebin -s logic_server -s inets -config my_server