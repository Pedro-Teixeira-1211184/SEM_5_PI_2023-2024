import {Inject, Service} from 'typedi';
import e, {Response, Request, NextFunction} from 'express';
import config from "../../config";
import IElevatorService from '../services/IServices/IElevatorService';
import IBuildingService from '../services/IServices/IBuildingService';
import IFloorService from '../services/IServices/IFloorService';
import IElevatorController from './IControllers/IElevatorController';
import IElevatorDTO from '../dto/IElevatorDTO';
import {Result} from '../core/logic/Result';
import {StatusCodes} from "http-status-codes";
import { min } from 'lodash';
import IBuildingDTO from '../dto/IBuildingDTO';


@Service()

export default class ElevatorController implements IElevatorController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
    @Inject(config.services.elevator.name) private elevatorServiceInstance: IElevatorService,
    @Inject(config.services.building.name) private buildingServiceInstance: IBuildingService,
    @Inject(config.services.floor.name) private floorServiceInstance: IFloorService
  ) {
  }

  public async createElevator(req: Request, res: Response, next: NextFunction) {
    try {
      const buildingOrError = await this.buildingServiceInstance.getBuildingByCode(req.body.buildingCode) as Result<IBuildingDTO>;
      if (buildingOrError.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json("Building not found");
      }

      const elevatorOrError = await this.elevatorServiceInstance.createElevator(req.body as IElevatorDTO) as Result<IElevatorDTO>;

      if (elevatorOrError.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json("Elevator already exists");
      }

      const elevatorDTO = elevatorOrError.getValue();
      return res.json(elevatorDTO).status(StatusCodes.ACCEPTED);
    } catch (e) {
      return next(e);
    }
  };

  public async findElevatorsByBuildingCode(req: Request, res: Response, next: NextFunction) {
    try {
      const result = await this.elevatorServiceInstance.getElevatorsByBuildingCode(req.params.buildingCode);
      if (result.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).send(result.errorValue());
      }
      return res.json(result.getValue()).status(StatusCodes.OK);
    }catch (e) {
      return next(e);
    }
  }


  public async getAll(req: Request, res: Response, next: NextFunction) {
    try {
      const result = await this.elevatorServiceInstance.getAll();
      if (result.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).send(result.errorValue());
      }
      return res.json(result.getValue()).status(StatusCodes.OK);
    }catch (e) {
      return next(e);
    }
  }
}
