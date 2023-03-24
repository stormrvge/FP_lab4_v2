import subprocess
import time

def main():
    chat_server_process = subprocess.Popen(["erl", "-sname", "test_server", "-run", "chat_server", "start"])
    time.sleep(5)  # Give the server some time to start

    try:
        # Perform a simple test by sending a message
        send_message = subprocess.Popen(
            ["erl", "-sname", "test_client", "-eval",
             "io:format(chat_server:send(\"Hello, chat server!\"))."],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
        )
        stdout, stderr = send_message.communicate()
        assert "ok" in stdout, "Sending a message to the chat server failed"

        # Perform a test to view message history
        print_history = subprocess.Popen(
            ["erl", "-sname", "test_client", "-eval",
             "io:format(chat_server:print_history())."],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
        )
        stdout, stderr = print_history.communicate()
        assert "The history" in stdout, "Fetching message history from the chat server failed"
    finally:
        chat_server_process.terminate()

if __name__ == "__main__":
    main()
