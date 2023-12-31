import {Inject, Service} from 'typedi';
import e, {Response, Request, NextFunction} from 'express';
import config from "../../config";
import IBuildingService from "../services/IServices/IBuildingService";
import IBuildingController from "./IControllers/IBuildingController";
import IBuildingDTO from '../dto/IBuildingDTO';
import {Result} from '../core/logic/Result';
import {StatusCodes} from "http-status-codes";
import { min } from 'lodash';

@Service()
export default class BuildingController implements IBuildingController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
    @Inject(config.services.building.name) private buildingServiceInstance: IBuildingService
  ) {
  }

  public async createBuilding(req: Request, res: Response, next: NextFunction) {
    try {
      const buildingOrError = await this.buildingServiceInstance.createBuilding(req.body as IBuildingDTO) as Result<IBuildingDTO>;

      if (buildingOrError.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json(buildingOrError.error);
      }

      const buildingDTO = buildingOrError.getValue();
      return res.json(buildingDTO).status(StatusCodes.ACCEPTED);
    } catch (e) {
      return next(e);
    }
  };

  public async updateBuilding(req: Request, res: Response, next: NextFunction) {
    try {
      const buildingOrError = await this.buildingServiceInstance.updateBuilding(req.body as IBuildingDTO) as Result<IBuildingDTO>;

      if (buildingOrError.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json(buildingOrError.error);
      }

      const buildingDTO = buildingOrError.getValue();
      return res.json(buildingDTO).status(StatusCodes.ACCEPTED);
    } catch (e) {
      return next(e);
    }
  }

  public async getAll(req: Request, res: Response, next: NextFunction) {
    try {

      const buildings = await this.buildingServiceInstance.getAll();

      if (buildings.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json(buildings.error);
      }

      return res.json(buildings.getValue()).status(StatusCodes.ACCEPTED);
    } catch (e) {
      return next(e);
    }
  }

  public async findByMinMaxFloors(req: Request, res: Response, next: NextFunction) {
    try {
      const range = req.params.range;
      const splitrange = range.split('-');
      const minFloors = Number(splitrange[0]);
      const maxFloors = Number(splitrange[1]);

      if(minFloors > maxFloors) {
        console.log("minFloors must be less than maxFloors");
        return null;
      }

      const buildings = await this.buildingServiceInstance.findByMinMaxFloors(minFloors, maxFloors);

      if (buildings.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json(buildings.error);
      }

      return res.json(buildings.getValue()).status(StatusCodes.ACCEPTED);
    } catch (e) {
      console.log('Error in BuildingController.findByMinMaxFloors(): ', e);
      return next(e);
    }
  }

}
