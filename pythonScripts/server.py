UDP_IP = "192.168.0.246"
UDP_PORT = 5005
import socket
import threading
import queue
import time
from app import generate

def main():
	request_queue = queue.Queue()

	def process_request(request):
    	generate()

	def process_queue(queue):
    	while True:
        	request = queue.get()
        	process_request(request)
        	queue.task_done()

	if __name__ == "__main__":
    	server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    	server_socket.bind((UDP_IP, UDP_PORT))
    	server_socket.listen(5)

    	processing_thread = threading.Thread(target=process_queue, args=(request_queue,))
    	processing_thread.start()

    	print("Aplicativo iniciado. Aguardando requisições...")

    	while True:
        	client_socket, address = server_socket.accept()
        	print(f"Requisição recebida de: {address}")

        	request = client_socket.recv(1024).decode()

        	request_queue.put(request)

        	response = "Requisição recebida e colocada na fila"
        	client_socket.send(response.encode())

        	client_socket.close()

    	processing_thread.join()
    	client_socket.close()
