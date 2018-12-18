import pandas as pd
from kafka import KafkaProducer
import json
import uuid
import datetime

# start the kafka producer on localhost
producer = KafkaProducer(
  bootstrap_servers='localhost:9092',
  value_serializer=lambda m: json.dumps(m).encode('ASCII')
)

# read in a day's aggregation of hits
df = pd.read_csv(
  '../data/2015_01_en_clickstream.tsv',
  sep='\t',
  header=0,
  names=['prev_id', 'curr_id', 'n', 'prev', 'curr'],
  usecols=['prev_id', 'n', 'prev', 'curr']
)

# exclude external referrers (e.g. google)
df.dropna(inplace=True)
df.drop(columns=['prev_id'], inplace=True)

# subset to popular pages (for simulation purposes)
df_subset = df[df.n >= 1000]

# simulate a hit by sampling with probability n from the 
# aggregation of hits
def log_hit():
  hit = df_subset.sample(n=1, weights='n', axis=0)
  res = producer.send(
    'wikipedia',
    key=str(uuid.uuid4()).encode(),
    value={
      'prev': hit.prev.item(),
      'curr': hit.curr.item(),
      'ts': str(datetime.datetime.now())[:19]
    }
  )

while True:
  log_hit()
