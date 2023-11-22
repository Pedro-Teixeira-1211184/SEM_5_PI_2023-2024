import {Inject, Service} from 'typedi';
import {Response, Request, NextFunction} from 'express';
import config from "../../config";
import IRoomService from "../services/IServices/IRoomService";
import IRoomController from "./IControllers/IRoomController";
import IRoomDTO from '../dto/IRoomDTO';
import {Result} from '../core/logic/Result';
import {StatusCodes} from "http-status-codes";
import { min } from 'lodash';

@Service()
export default class RoomController implements IRoomController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
    @Inject(config.services.room.name) private RoomServiceInstance: IRoomService
  ) {
  }

  public async createRoom(req: Request, res: Response, next: NextFunction) {
    try {
      const RoomOrError = await this.RoomServiceInstance.createRoom(req.body as IRoomDTO) as Result<IRoomDTO>;

      if (RoomOrError.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json("Elevator already exists");
      }

      const RoomDTO = RoomOrError.getValue();
      return res.json(RoomDTO).status(StatusCodes.ACCEPTED);
    } catch (e) {
      return next(e);
    }
  }

  public async getRoomsByFloorCode(req: Request, res: Response, next: NextFunction) {
    try {
      const floorCode = req.params.floorCode;
      const rooms = await this.RoomServiceInstance.getRoomsByFloorCode(floorCode) as Result<IRoomDTO[]>
      if (rooms.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json(rooms.errorValue());
      }
      const roomsDTO = rooms.getValue();
      return res.json(roomsDTO).status(StatusCodes.OK);
    }catch (e) {
      return next(e);
    }
  }
}
