import socket
import time
UDP_IP = "192.168.0.246"
UDP_PORT = 5005
TIME_INTERVAL = 10

def send_request(request):    
    
    client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    client_socket.connect((UDP_IP, UDP_PORT))
    client_socket.send(request.encode())
    response = client_socket.recv(1024).decode()
    print(f"Response received: {response}")
    client_socket.close()

print("UDP target IP: %s"% UDP_IP)
print("UDP target PORT: %s"% UDP_PORT)

start = time.time()
while True:
    t = (time.time() - start)/60
