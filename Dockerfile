# pitboard paid-journal site image (single origin: landing, journal,
# subscribe/pay flow, editorial console)
FROM racket/racket:9.3

RUN raco pkg install --auto --no-docs http-easy feed && \
    raco pkg install --auto --no-docs https://github.com/turlamai/commerce.git

WORKDIR /app
COPY . .
RUN raco pkg install --auto --no-docs --link /app

EXPOSE 8090
VOLUME /data
CMD ["raco", "pitboard", "serve", "--port", "8090", "--data", "/data"]
