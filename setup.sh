~/Code/confluent/bin/confluent start

~/Code/confluent/bin/kafka-topics --create --zookeeper localhost:2181 --replication-factor 1 --partitions 2 --topic wikipedia

~/Code/confluent/bin/kafka-console-consumer --bootstrap-server localhost:9092 --topic wikipedia

elasticsearch
kibana

~/Code/confluent/bin/confluent load es-sink-wikipedia -d code/es_connector.json
