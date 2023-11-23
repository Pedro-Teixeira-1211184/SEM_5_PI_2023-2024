import {Inject, Service} from 'typedi';
import {Response, Request, NextFunction} from 'express';
import config from "../../config";
import IMapController from "./IControllers/IMapController";
import IMapService from "../services/IServices/IMapService";
import {StatusCodes} from "http-status-codes";
import IMapDTO from "../dto/IMapDTO";
import {Map} from "../domain/map";
import {MapMapper} from "../mappers/MapMapper";
import {Result} from "../core/logic/Result";
import IPathDTO from "../dto/IPathDTO";
import IPathResultDTO from "../dto/IPathResultDTO";
import IPlantDTO from "../dto/IPlantDTO";

@Service()

export default class MapController implements IMapController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
    @Inject(config.services.map.name) private mapServiceInstance: IMapService
  ) {
  }

  public async createMap(req: Request, res: Response, next: NextFunction) {
    try {
      const mapOrError = await this.mapServiceInstance.createMap(req.body as IMapDTO) as Result<IMapDTO>;

      if (mapOrError.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json(mapOrError.errorValue());
      }

      const mapDTO = mapOrError.getValue();
      return res.json(mapDTO).status(StatusCodes.ACCEPTED);
    } catch (e) {
      return next(e);
    }
  }

  public async loadMap(req: Request, res: Response, next: NextFunction) {
    const buildingCode = String(req.params.buildingCode);
    const floorNumber = Number(req.params.floorNumber);
    try {
      const mapOrError = await this.mapServiceInstance.loadMap(buildingCode, floorNumber) as Result<IMapDTO>;

      if (mapOrError.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json(mapOrError.errorValue());
      }

      const mapDTO = mapOrError.getValue();
      const plant = await this.mapServiceInstance.turnToPlant(mapDTO);
      return res.json(plant).status(StatusCodes.ACCEPTED);
    } catch (e) {
      return next(e);
    }
  }

  public async listMaps(req: Request, res: Response, next: NextFunction) {
    try {
      const mapOrError = await this.mapServiceInstance.listMaps() as Result<IPlantDTO[]>;

      if (mapOrError.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json(mapOrError.errorValue());
      }

      const mapDTO = mapOrError.getValue();
      return res.json(mapDTO).status(StatusCodes.ACCEPTED);
    } catch (e) {
      return next(e);
    }
  }

  public async pathBetweenFloors(req: Request, res: Response, next: NextFunction) {
    const origin = String(req.params.origin);
    const destination = String(req.params.destination);
    const path: IPathDTO = {origin, destination};

    try {
      const pathOrError = await this.mapServiceInstance.pathBetweenFloors(path) as Result<IPathResultDTO>;

      if (pathOrError.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json(pathOrError.errorValue());
      }

      const pathDTO = pathOrError.getValue();
      return res.json(pathDTO).status(StatusCodes.ACCEPTED);
    } catch (e) {
      return next(e);
    }
  }

  public async getAll(req: Request, res: Response, next: NextFunction) {
    try {
      const maps = await this.mapServiceInstance.getAll() as Result<IMapDTO[]>;
      if (maps.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json(maps.errorValue());
      }
      const mapDTO = maps.getValue();
      return res.json(mapDTO).status(StatusCodes.ACCEPTED);
    } catch (e) {
      return next(e);
    }
  }

}
