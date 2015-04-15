#include <sys/socket.h>
#include <netinet/in.h>
#include <fcntl.h>
#include <unistd.h>

typedef int socket_t;

socket_t socket_open ()
{
    int s, broadcastValue = 1;

    s = socket (AF_INET, SOCK_DGRAM, IPPROTO_UDP);

    if (s <= 0) {
        return s;
    }

    if (fcntl (s, F_SETFL, O_NONBLOCK, 1) == -1) {
        return -1;
    }

    setsockopt (s, SOL_SOCKET, SO_BROADCAST, (char*)&broadcastValue, sizeof (int));

    return s;
}

int socket_bind (socket_t handle, unsigned long int host, unsigned short port)
{
    struct sockaddr_in addr;

    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl (host);
    addr.sin_port = htons (port);

    if (bind (handle, (struct sockaddr*)&addr, sizeof (addr)) < 0) {
        return -1;
    }

    return 0;
}

void socket_close (socket_t handle)
{
    close (handle);
}

int socket_send (socket_t handle, unsigned long int host, unsigned short port, void* data, int data_size)
{
    struct sockaddr_in addr;

    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl (host);
    addr.sin_port = htons (port);

    int sent_size = sendto (handle, (const char*)data, data_size, 0, (const struct sockaddr*)&addr, sizeof (addr));

    if (sent_size != data_size) {
        return -1;
    }

    return 0;
}

int socket_receive (socket_t handle, unsigned long int* host, unsigned short* port, void* data, int data_size)
{
    socklen_t addr_len;
    int received_size;
    struct sockaddr_in addr;

    addr_len = sizeof (addr);
    received_size = recvfrom (handle, (char*)data, data_size, 0, (struct sockaddr*)&addr, &addr_len);

    if (received_size <= 0) {
        return 0;
    }

    *host = ntohl (addr.sin_addr.s_addr);
    *port = ntohs (addr.sin_port);

    return received_size;
}
