import subprocess
import time


def main():
    chat_server_process = subprocess.Popen(["erl", "-sname", "test_server", "-run", "chat_server", "start"])
    time.sleep(5)  # Give the server some time to start

    try:
        # Perform a simple test by sending a message
        send_message = subprocess.run(
            ["erl", "-sname", "test_client", "-eval",
             "io:format(chat_server:send(\"Hello, chat server!\"))."],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=10
        )

        assert "ok" in send_message.stdout, "Sending a message to the chat server failed"
    finally:
        chat_server_process.terminate()


if __name__ == "__main__":
    main()