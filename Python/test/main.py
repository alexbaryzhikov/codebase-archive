import numpy as np
import os
import tensorflow as tf
from keras.models import Sequential
from keras.layers import Dense

os.environ['TF_CPP_MIN_LOG_LEVEL'] = '1'

# Validate TensorFlow
hello = tf.constant('Hello, TensorFlow!')
session = tf.Session()
print(session.run(hello).decode())
session.close()

# Build sequential model
model = Sequential()
model.add(Dense(units=64, activation='relu', input_dim=100))
model.add(Dense(units=10, activation='softmax'))
model.compile(loss='categorical_crossentropy', optimizer='sgd', metrics=['accuracy'])

# Generate dummy data
data = np.random.random((1000, 100))
labels = np.random.randint(10, size=(1000, 10))

model.fit(data, labels, epochs=10, batch_size=32)

loss_and_metrics = model.evaluate(data, labels, batch_size=128)
print()
print(loss_and_metrics)
