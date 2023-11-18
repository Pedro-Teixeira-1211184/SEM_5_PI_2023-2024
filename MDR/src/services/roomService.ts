import {Service, Inject} from 'typedi';
import config from "../../config";
import IRoomDTO from '../dto/IRoomDTO';
import {Room} from "../domain/room";
import IRoomRepo from "../services/IRepos/IRoomRepo";
import IRoomService from "./IServices/IRoomService";
import {Result} from "../core/logic/Result";
import {RoomMapper} from "../mappers/RoomMapper";
import IFloorRepo from "./IRepos/IFloorRepo";



@Service()

export default class RoomService implements IRoomService {

  constructor(
    @Inject(config.repos.room.name) private roomRepo: IRoomRepo,
    @Inject(config.repos.floor.name) private floorRepo: IFloorRepo
  ) {
  }

  public async createRoom(roomDTO: IRoomDTO): Promise<Result<IRoomDTO>> {
    try {
      //verify if floor exists
      const floorExists = await this.floorRepo.findByDomainId(roomDTO.floorCode);
      if (floorExists == null) {
        console.log('Floor does not exist');
        return Result.fail<IRoomDTO>('Floor does not exist');
      }


      const roomOrError = await Room.create(roomDTO);
      if (roomOrError.isFailure) {
        return Result.fail<IRoomDTO>(roomOrError.errorValue());
      }

      const roomResult = roomOrError.getValue();

      //save Room
      const roomCreated = await this.roomRepo.save(roomResult);

      if (roomCreated === null) {
        return Result.fail<IRoomDTO>('Room not created');
      }

      const roomDTOResult = RoomMapper.toDTO(roomResult) as IRoomDTO;
      return Result.ok<IRoomDTO>(roomDTOResult)
    } catch (e) {
      throw e;
    }
  }

  public async getRoomsByFloorCode(floorCode: string): Promise<Result<IRoomDTO[]>> {
    try {
      //verify if floor exists
      const floorExists = await this.floorRepo.findByDomainId(floorCode);
      if (floorExists == null) {
        console.log('Floor does not exist');
        return Result.fail<IRoomDTO[]>('Floor does not exist');
      }

      const rooms = await this.roomRepo.findByFloorCode(floorCode);
      if (rooms == null) {
        return Result.fail<IRoomDTO[]>('Rooms not found');
      }

      const roomsDTO: IRoomDTO[] = [];
      for (let i = 0; i < rooms.length; i++) {
        roomsDTO.push(RoomMapper.toDTO(rooms[i]));
      }

      return Result.ok<IRoomDTO[]>(roomsDTO);

    }catch (e) {
      throw e;
    }
  }
}
