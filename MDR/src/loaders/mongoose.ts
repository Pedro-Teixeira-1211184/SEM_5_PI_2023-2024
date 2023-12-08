import mongoose, { Connection } from 'mongoose';
import config from '../../config';

async function connectToDatabase(): Promise<Connection> {
  try {
    await mongoose.connect(config.databaseURL, {
      user: 'mongoadmin',
      pass: '1593943c6daffc4c4b671b2a',
    });
    return mongoose.connection;
  } catch (error) {
    throw error;
  }
}

export default connectToDatabase;
