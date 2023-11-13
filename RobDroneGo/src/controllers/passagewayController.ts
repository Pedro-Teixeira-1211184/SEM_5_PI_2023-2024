import {Inject, Service} from 'typedi';
import e, {Response, Request, NextFunction} from 'express';
import config from "../../config";
import IPassagewayService from "../services/IServices/IPassagewayService";
import IPassagewayController from "./IControllers/IPassagewayController";
import IPassagewayDTO from '../dto/IPassagewayDTO';
import {Result} from '../core/logic/Result';
import {StatusCodes} from "http-status-codes";
import IFloorService from '../services/IServices/IFloorService';
import IFloorDTO from '../dto/IFloorDTO';


@Service()
export default class PassagewayController implements IPassagewayController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
    @Inject(config.services.passageway.name) private passagewayServiceInstance: IPassagewayService,
    @Inject(config.services.floor.name) private floorServiceInstance: IFloorService
  ) {
  }

  public async createPassageway(req: Request, res: Response, next: NextFunction) {
    try {

      const passagewayOrError = await this.passagewayServiceInstance.createPassageway(req.body as IPassagewayDTO) as Result<IPassagewayDTO>;

      if (passagewayOrError.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json(passagewayOrError.error);
      }

      const passagewayDTO = passagewayOrError.getValue();
      return res.json(passagewayDTO).status(StatusCodes.ACCEPTED);

    } catch (e) {
      return next(e);
    }
  };

  public async getPassagewaysInBuildings(req: Request, res: Response, next: NextFunction) {
    try {
      const floorOrError = await this.floorServiceInstance.findFloorsByBuildingCode(req.params.buildingCode1) as Result<Array<IFloorDTO>>;
      const floorOrError1 = await this.floorServiceInstance.findFloorsByBuildingCode(req.params.buildingCode2) as Result<Array<IFloorDTO>>;
  
      if (floorOrError.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json("First building selected doesn t have any floors registered");
      }
      
      if (floorOrError1.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json("Second building selected doesn t have any floors registered");
      }

      const floorsResult = floorOrError.getValue();
      const floorsResult1 = floorOrError1.getValue();

      const passagewayOrError = await this.passagewayServiceInstance.getPassagewaysInBuildings(floorsResult,floorsResult1) as Result<Array<IPassagewayDTO>>;

      if (passagewayOrError.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json("No passageways found");
      }

      const passagewayDTO = passagewayOrError.getValue();
      return res.json(passagewayDTO).status(StatusCodes.ACCEPTED);

    } catch (e) {
      return next(e);
    }
  };
  
  public async updatePassageway(req: Request, res: Response, next: NextFunction) {
    try{ 
      const floorOrError = await this.floorServiceInstance.findFloorByCode(req.body.floorCode1) as Result<IFloorDTO>;
      const floorOrError1 = await this.floorServiceInstance.findFloorByCode(req.body.floorCode2) as Result<IFloorDTO>;
      if (floorOrError.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json("Unable to find floor1");
      }

      if (floorOrError1.isFailure) {
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json("Unable to find floor2");
      }

      const passagewayOrError = await this.passagewayServiceInstance.updatePassageway(req.body as IPassagewayDTO) as Result<IPassagewayDTO>;

      if(passagewayOrError.isFailure){
        return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json(passagewayOrError.error);
      }
  
      const passagewayDTO = passagewayOrError.getValue();
      return res.json(passagewayDTO).status(StatusCodes.ACCEPTED);
    }catch(e){
      return next(e);
    }
  }

}
