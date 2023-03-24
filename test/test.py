import subprocess
import time


def main():
    # Start an Erlang shell
    erl_process_discovery = subprocess.Popen(['erl', '-sname', 'discovery', "-cookies", "1234"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    erl_process_alice = subprocess.Popen(['erl', '-sname', 'alice', "-cookies", "1234"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    erl_process_bob = subprocess.Popen(['erl', '-sname', 'bob', "-cookies", "1234"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    erl_process_charlie = subprocess.Popen(['erl', '-sname', 'charlie', "-cookies", "1234"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    init_nodes(erl_process_discovery, erl_process_alice, erl_process_bob, erl_process_charlie)
    time.sleep(1)

    output, error = erl_process_discovery.communicate()
    print("Discovery output:")
    print(output.decode('utf-8'))
    print("\n\n\n")

    output, error = erl_process_alice.communicate()
    print("Alice output:")
    print(output.decode('utf-8'))
    print("\n\n\n")

    output, error = erl_process_bob.communicate()
    print("Bob output:")
    print(output.decode('utf-8'))
    print("\n\n\n")

    output, error = erl_process_charlie.communicate()
    print("Charlie output:")
    print(output.decode('utf-8'))

    time.sleep(3)
    wait_for_clients_exit(erl_process_discovery, erl_process_alice, erl_process_bob, erl_process_charlie)


def init_nodes(erl_process_discovery, erl_process_alice, erl_process_bob, erl_process_charlie):
    erl_process_discovery.stdin.write(b'discovery_server:start().\n')
    time.sleep(2)

    # Send a command to the Erlang shell and read the output
    erl_process_alice.stdin.write(b'chat_server:start().\n')
    erl_process_alice.stdin.flush()
    time.sleep(2)

    # Send a command to the Erlang shell and read the output
    erl_process_bob.stdin.write(b'chat_server:start().\n')
    erl_process_bob.stdin.flush()
    time.sleep(2)

    # Send a command to the Erlang shell and read the output
    erl_process_charlie.stdin.write(b'chat_server:start().\n')
    erl_process_charlie.stdin.flush()
    time.sleep(2)


def wait_for_clients_exit(erl_process_discovery, erl_process_alice, erl_process_bob, erl_process_charlie):
    erl_process_discovery.wait()
    erl_process_alice.wait()
    erl_process_bob.wait()
    erl_process_charlie.wait()


if __name__ == "__main__":
    main()