import {Inject, Service} from 'typedi';
import {Response, Request, NextFunction} from 'express';
import config from "../../config";
import IBuildingService from "../services/IServices/IBuildingService";
import IFloorController from "./IControllers/IFloorController";
import {StatusCodes} from "http-status-codes";
import IFloorService from "../services/IServices/IFloorService";
import IFloorDTO from "../dto/IFloorDTO";
import {Result} from "../core/logic/Result";
import IBuildingDTO from "../dto/IBuildingDTO";

@Service()
export default class FloorController implements IFloorController /* TODO: extends ../core/infra/BaseController */ {
    constructor(
        @Inject(config.services.floor.name) private floorServiceInstance: IFloorService,
        @Inject(config.services.building.name) private buildingServiceInstance: IBuildingService,
    ) {
    }

    public async createFloor(req: Request, res: Response, next: NextFunction) {
        try {
            //check if building exists by buildingCode
            const buildingOrError = await this.buildingServiceInstance.getBuildingByCode(req.body.buildingCode) as Result<IBuildingDTO>;

            if (buildingOrError.isFailure) {
                return res.status(StatusCodes.INTERNAL_SERVER_ERROR).send(buildingOrError.errorValue());
            }

            //check if floor number is valid
            if (Number(req.body.number) > buildingOrError.getValue().maxFloors) {
                console.log("Floor number is invalid!");
                return res.status(StatusCodes.INTERNAL_SERVER_ERROR).send("Floor number is higher than maximum number of floors!");
            }

            const floorOrError = await this.floorServiceInstance.createFloor(req.body as IFloorDTO) as Result<IFloorDTO>;
            if (floorOrError.isFailure) {
                console.log("Floor not created!");
                return res.status(StatusCodes.INTERNAL_SERVER_ERROR).send(floorOrError.errorValue());
            }

            const floorResult = floorOrError.getValue();
            return res.status(StatusCodes.CREATED).json(floorResult);
        } catch (e) {
            return next(e);
        }
    }

    public async findFloorsByBuildingCode(req: Request, res: Response, next: NextFunction) {
        try {
            const floorOrError = await this.floorServiceInstance.findFloorsByBuildingCode(req.params.buildingCode) as Result<IFloorDTO[]>;
            if (floorOrError.isFailure) {
                return res.status(StatusCodes.INTERNAL_SERVER_ERROR).send(floorOrError.errorValue());
            }

            const floorResult = floorOrError.getValue();
            return res.status(StatusCodes.OK).json(floorResult);
        } catch (e) {
            return next(e);
        }
    }

    public async updateFloor(req: Request, res: Response, next: NextFunction) {
    try{
        const floorOrError = await this.floorServiceInstance.updateFloor(req.body as IFloorDTO) as Result<IFloorDTO>;
        if (floorOrError.isFailure) {
            return res.status(StatusCodes.INTERNAL_SERVER_ERROR).send(floorOrError.errorValue());
        }

        const floorDTO = floorOrError.getValue();
        return res.json(floorDTO).status(StatusCodes.ACCEPTED);
    }catch(e){
        return next(e);
    }    
    }
    
}

