import {Service, Inject} from 'typedi';
import config from "../../config";
import IPassagewayDTO from '../dto/IPassagewayDTO';
import {Passageway} from "../domain/passageway";
import IPassagewayRepo from "../services/IRepos/IPassagewayRepo";
import IPassagewayService from "./IServices/IPassagewayService";
import {Result} from "../core/logic/Result";
import {PassagewayMapper} from "../mappers/PassagewayMapper";
import IFloorRepo from "./IRepos/IFloorRepo";
import IFloorDTO from '../dto/IFloorDTO';


@Service()

export default class PassagewayService implements IPassagewayService {

  constructor(
    @Inject(config.repos.passageway.name) private PassagewayRepo: IPassagewayRepo,
    @Inject(config.repos.floor.name) private FloorRepo: IFloorRepo
  ) {
  }

  public async createPassageway(PassagewayDTO: IPassagewayDTO): Promise<Result<IPassagewayDTO>> {
    try {

      const PassagewayOrError = await Passageway.create(PassagewayDTO);

      if (PassagewayOrError.isFailure) {
        return Result.fail<IPassagewayDTO>(PassagewayOrError.errorValue());
      }

      const PassagewayResult = PassagewayOrError.getValue();

      //very if the floors exist

      const floor1 = await this.FloorRepo.findByDomainId(PassagewayResult.floorCode1);

      if (floor1 == null) {
        return Result.fail<IPassagewayDTO>('Floor1 not found');
      }
      const floor2 = await this.FloorRepo.findByDomainId(PassagewayResult.floorCode2);

      if (floor2 == null) {
        return Result.fail<IPassagewayDTO>('Floor2 not found');
      }

      //save Passageway
      const PassagewayCreated = await this.PassagewayRepo.save(PassagewayResult);

      if (PassagewayCreated === null) {
        return Result.fail<IPassagewayDTO>('Passageway not created');
      }

      const PassagewayDTOResult = PassagewayMapper.toDTO(PassagewayResult) as IPassagewayDTO;
      return Result.ok<IPassagewayDTO>(PassagewayDTOResult)

    } catch (e) {
      throw e;
    }

  }

  public async findFloorsInPassageways(floorCode: string): Promise<Result<boolean>> {
    try {
      const floorExistsInPassageways = await this.PassagewayRepo.findFloorsInPassageways(floorCode);

      if (floorExistsInPassageways == false) {
        return Result.fail<boolean>('No passageways found');
      }

      return Result.ok<boolean>(floorExistsInPassageways)
    } catch (e) {
      console.log('Error in PassagewayService.findFloorsInPassageways', e);
      throw e;
    }
  }

  public async getPassagewaysInBuildings(floors1: Array<IFloorDTO>, floors2: Array<IFloorDTO>): Promise<Result<Array<IPassagewayDTO>>> {
    try {
      const passagewaysInBuildings = await this.PassagewayRepo.getPassagewaysInBuildings(floors1, floors2);

      if (passagewaysInBuildings.length == 0) {
        return Result.fail<Array<IPassagewayDTO>>('No passageways found');
      }

      return Result.ok<Array<IPassagewayDTO>>(passagewaysInBuildings)
    } catch (e) {
      console.log('Error in PassagewayService.getPassagewaysInBuildings', e);
      throw e;
    }
  }

  public async updatePassageway(PassagewayDTO: IPassagewayDTO): Promise<Result<IPassagewayDTO>> {
    try{
      const updatedPassageway = await this.PassagewayRepo.update(PassagewayDTO.floorCode1, PassagewayDTO.floorCode2, PassagewayDTO);
      if (updatedPassageway == null) {
        console.log('Passageway not found');
        return Result.fail<IPassagewayDTO>('Passageway not found');
      }

      const updatedPassagewayDTO = PassagewayMapper.toDTO(updatedPassageway);
      return Result.ok<IPassagewayDTO>(updatedPassagewayDTO);
    } catch (e) {
      throw e;
    }
  }


}
