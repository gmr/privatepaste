FROM ubuntu:latest
COPY privatepaste.tar /tmp/privatepaste.tar
RUN tar -C /usr/local -xvf /tmp/privatepaste.tar && \
    mkdir -p /var/log/privatepaste /var/lib/privatepaste/mnesia && \
    rm /tmp/privatepaste.tar && \
    echo "VSSRTIKSRMKWXYPDLJKX" > /root/.erlang.cookie && \
    chown -R root:root /usr/local/privatepaste
WORKDIR /usr/local/privatepaste
EXPOSE 8000
EXPOSE 4369 9500 9501 9502 9503 9504 9505 9506 9507 9508 9509
VOLUME ["/var/lib/privatepaste", "/var/log/privatepaste"]
CMD ["/usr/local/privatepaste/bin/privatepaste", "foreground"]
