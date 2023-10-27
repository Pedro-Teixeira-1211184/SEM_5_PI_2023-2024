import {Inject, Service} from 'typedi';

import IRoomRepo from "../services/IRepos/IRoomRepo";
import {Room} from "../domain/room";
import {RoomMapper} from "../mappers/RoomMapper";

import {Document, FilterQuery, Model} from "mongoose";
import {IRoomPersistence} from "../dataschema/IRoomPersistence";


@Service()
export default class RoomRepo implements IRoomRepo {

  constructor(
    @Inject('roomSchema') private roomSchema: Model<IRoomPersistence & Document>,
  ) {
  }

  private createBaseQuery(): any {
    return {
      where: {},
    }
  }

  public async save(room: Room): Promise<Room> {


    try {
      if (await this.exists(room)) {

        const rawRoom: any = RoomMapper.toPersistence(room);

        const RoomCreated = await this.roomSchema.create(rawRoom);

        return RoomMapper.toDomain(RoomCreated);
      } else {
        console.log('Room already exists');
        return null;
      }
    } catch (error) {
      throw error;
    }
  }

  public async findByDomainId(roomId: string): Promise<Room> {
    try {
      const query = {roomID: roomId};
      const roomRecord = await this.roomSchema.findOne(query as FilterQuery<IRoomPersistence & Document>);
      return RoomMapper.toDomain(roomRecord);
    } catch (error) {
      throw error;
    }
  }

  public async delete(room: Room): Promise<void> {
    try {
      const query = {roomID: room.id.toString()};
      await this.roomSchema.deleteOne(query as FilterQuery<IRoomPersistence & Document>);
    } catch (error) {
      throw error;
    }
  }

  public async exists(room: Room): Promise<boolean> {
    try {
      //determines if the Room exists by its unique name
      const query = {roomCode: room.name.toString()};
      const RoomDocument = await this.roomSchema.findOne(query as FilterQuery<IRoomPersistence & Document>);
      return RoomDocument == null;
    } catch (error) {
      throw error;
    }
  }
}
