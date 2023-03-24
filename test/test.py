import subprocess
import re
import time


def main():
    # Start an Erlang shell
    erl_process_discovery = subprocess.Popen(['erl', '-sname', 'discovery'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    erl_process_alice = subprocess.Popen(['erl', '-sname', 'alice'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    erl_process_bob = subprocess.Popen(['erl', '-sname', 'bob'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    erl_process_charlie = subprocess.Popen(['erl', '-sname', 'charlie'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    init_nodes(erl_process_discovery, erl_process_alice, erl_process_bob, erl_process_charlie)
    time.sleep(1)
    # create_group(erl_process_bob, "group1")
    # time.sleep(1)
    # send_message(erl_process_bob, "group1", "cat")
    # send_message(erl_process_bob, "group1", "dog")
    # send_message(erl_process_bob, "group1", "banana")
    # send_message(erl_process_alice, "group1", "cow")
    # send_message(erl_process_alice, "group1", "just message")
    # send_message(erl_process_charlie, "group1", "this is p2p chat")
    # send_message(erl_process_charlie, "group1", "orange")
    #
    # create_group(erl_process_bob, "group2")
    # time.sleep(1)
    # send_message(erl_process_bob, "group2", "i want to live")
    # send_message(erl_process_bob, "group2", "haha")
    # send_message(erl_process_bob, "group2", "qwerty")
    # send_message(erl_process_alice, "group2", "hello hello hello hello hello")
    # send_message(erl_process_alice, "group2", "another one message")
    # send_message(erl_process_charlie, "group2", "im fine")
    # send_message(erl_process_charlie, "group2", "really")
    #
    # create_group(erl_process_bob, "group3")
    # time.sleep(1)
    # send_message(erl_process_bob, "group3", "message in third group")
    # send_message(erl_process_bob, "group3", "i am bob")
    # send_message(erl_process_bob, "group3", "qwerty")
    # send_message(erl_process_alice, "group3", "i am alice")
    # send_message(erl_process_alice, "group3", "haha")
    # send_message(erl_process_charlie, "group3", "haha haha haha")
    # send_message(erl_process_charlie, "group3", "qwerty qwerty")
    #
    # group_list(erl_process_alice)
    # group_list(erl_process_bob)
    # group_list(erl_process_charlie)
    #
    # list_users(erl_process_alice, "group1")
    # list_users(erl_process_alice, "group2")
    # list_users(erl_process_alice, "group3")
    #
    # list_users(erl_process_bob, "group1")
    # list_users(erl_process_bob, "group2")
    # list_users(erl_process_bob, "group3")
    #
    # list_users(erl_process_charlie, "group1")
    # list_users(erl_process_charlie, "group2")
    # list_users(erl_process_charlie, "group3")
    #
    # view_history(erl_process_alice, "group1")
    # view_history(erl_process_alice, "group2")
    # view_history(erl_process_alice, "group3")
    #
    # view_history(erl_process_bob, "group1")
    # view_history(erl_process_bob, "group2")
    # view_history(erl_process_bob, "group3")
    #
    # view_history(erl_process_charlie, "group1")
    # view_history(erl_process_charlie, "group2")
    # view_history(erl_process_charlie, "group3")

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


def create_group(client, group_name):
    client.stdin.write('chat_client:createGroup({}).\n'.format(group_name).encode())
    client.stdin.flush()


def send_message(client, group_name, message):
    client.stdin.write('chat_client:sendMessage({}, "{}").\n'.format(group_name, message).encode())
    client.stdin.flush()


def view_history(client, group_name):
    client.stdin.write('chat_client:viewHistory({}).\n'.format(group_name).encode())
    client.stdin.flush()


def group_list(client):
    client.stdin.write(b'chat_client:groupList().\n')
    client.stdin.flush()


def list_users(client, group_name):
    client.stdin.write('chat_client:listUsers({}).\n'.format(group_name).encode())
    client.stdin.flush()


def parse_node(line_from_erlang_cmd):
    match = re.search(r"\((\w+@[\w-]+)\)", line_from_erlang_cmd)
    if match:
        node = match.group(1)
        return node
    else:
        return None


def get_command_line_info(client):
    line = ""
    while True:
        inner_line = client.stdout.readline()
        if not line:
            break
        line += inner_line
    return line


def parse_message(command_line_info):
    return re.findall(r"---START---\n(.|\n)*?---END OF CHAT HISTORY--", command_line_info)


if __name__ == "__main__":
    main()