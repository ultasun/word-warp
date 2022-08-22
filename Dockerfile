# Welcome to the word warp demo.

FROM alpine:3.16

# Update / Upgrade Alpine
RUN apk -U upgrade

RUN apk add clisp

# Create a non-privileged user for the runtime, home directory /app
RUN adduser --disabled-password -h /app -s /bin/bash -u 33333 clisp-user

# Switch the starting directory to /app
WORKDIR /app

# Install the files into the image
COPY . .

# Switch user away from root privilege
USER 33333

# Start CINDI
CMD [ "./run.sh" ]

# ----------------------
# End of the 'word-warp' Dockerfile
