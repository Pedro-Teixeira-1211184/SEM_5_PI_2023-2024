import mongoose, { Connection } from 'mongoose';
import config from '../../config';

async function connectToDatabase(): Promise<Connection> {
  try {
    await mongoose.connect(config.databaseURL, {
      user: 'mongoadmin',
      pass: 'efc6511ad2b66cbb9750c5c1',
    });
    return mongoose.connection;
  } catch (error) {
    throw error;
  }
}

export default connectToDatabase;
