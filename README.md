## P2P Chat.
Выполнил: Свистухин Андрей. Р34112.

P2P Chat - это Erlang приложение для обмена сообщениями между пользователями в реальном времени.
### API
1. start/0 - запуск сервера
2. send/1 - функция для отправки сообщения всем подключенным пользователям.
3. print_history/0 - Функция для вывода истории сообщений в консоль.
4. refresh_connections/2 - функция для обновления списка подключений пользователя User на Connections.
5. get_connections/1 - функция для получения списка подключенных пользователей у пользователя User.
6. handshake/1 - Функция для получения ключей от пользователя User.
7. leave/1 - Функция для отключения пользователя User.

### Примеры использования:
shell 1:
```
discovery_server:start().
```

shell 2:
```
(user2@127.0.0.1)1> chat_server:start().
{ok,<0.88.0>}
(user2@127.0.0.1)2> [2023-03-24|12:27:19] - From: <9883.88.0> Message: Hello user2
(user2@127.0.0.1)2> chat_server:send("Hello user1").
ok
(user2@127.0.0.1)3> [2023-03-24|12:27:29] - From: <9883.88.0> Message: Hello user22
(user2@127.0.0.1)3> chat_server:send("Hello user11"). 
ok

```

shell 3:
```
(user1@127.0.0.1)1> chat_server:start().             
{ok,<0.88.0>}
(user1@127.0.0.1)2> chat_server:send("Hello user2").
ok
(user1@127.0.0.1)3> [2023-03-24|12:27:22] - From: <9883.88.0> Message: Hello user1
(user1@127.0.0.1)3> chat_server:send("Hello user22").  
ok
(user1@127.0.0.1)4> [2023-03-24|12:27:32] - From: <9883.88.0> Message: Hello user11
ok
```

shell 4:
```
(user3@127.0.0.1)1> chat_server:start().
{ok,<0.88.0>}
(user3@127.0.0.1)2> chat_server:print_history().
The history
ok
(user3@127.0.0.1)3> [2023-03-24|12:27:19] From <9883.88.0> Message Hello user2 
(user3@127.0.0.1)3> [2023-03-24|12:27:32] From <9884.88.0> Message Hello user11
(user3@127.0.0.1)3> [2023-03-24|12:27:22] From <9884.88.0> Message Hello user1 
(user3@127.0.0.1)3> [2023-03-24|12:27:29] From <9883.88.0> Message Hello user22
```

### Скриншот работы приложения:
![Image alt](https://github.com/stormrvge/FP_lab4_v2/blob/master/img/console.png)
