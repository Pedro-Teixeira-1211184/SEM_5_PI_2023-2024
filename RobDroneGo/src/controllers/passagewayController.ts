import {Inject, Service} from 'typedi';
import e, {Response, Request, NextFunction} from 'express';
import config from "../../config";
import IPassagewayService from "../services/IServices/IPassagewayService";
import IPassagewayController from "./IControllers/IPassagewayController";
import IPassagewayDTO from '../dto/IPassagewayDTO';
import {Result} from '../core/logic/Result';
import {StatusCodes} from "http-status-codes";


@Service()
export default class PassagewayController implements IPassagewayController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
    @Inject(config.services.passageway.name) private passagewayServiceInstance: IPassagewayService
  ) {
  }

  public async createPassageway(req: Request, res: Response, next: NextFunction) {
    try {
      const passagewayOrError = await this.passagewayServiceInstance.createPassageway(req.body as IPassagewayDTO) as Result<IPassagewayDTO>;

      if (passagewayOrError.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json("passageway already exists");
      }

      const passagewayDTO = passagewayOrError.getValue();
      return res.json(passagewayDTO).status(StatusCodes.ACCEPTED);
    } catch (e) {
      return next(e);
    }
  };
} 