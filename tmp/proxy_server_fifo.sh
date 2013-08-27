mkfifo /tmp/proxy_server_fifo
nc -l -p 6868 <fifo | nc digisport.ro 80 >/tmp/proxy_server_fifo
