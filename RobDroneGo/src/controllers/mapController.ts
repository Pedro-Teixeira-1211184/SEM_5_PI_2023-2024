import {Inject, Service} from 'typedi';
import {Response, Request, NextFunction} from 'express';
import config from "../../config";
import IMapController from "./IControllers/IMapController";
import IMapService from "../services/IServices/IMapService";
import IBuildingDTO from "../dto/IBuildingDTO";
import {Result} from "../core/logic/Result";
import {StatusCodes} from "http-status-codes";
import IMapDTO from "../dto/IMapDTO";

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



}
