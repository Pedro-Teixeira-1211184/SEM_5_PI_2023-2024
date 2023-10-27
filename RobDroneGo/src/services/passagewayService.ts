import {Service, Inject} from 'typedi';
import config from "../../config";
import IPassagewayDTO from '../dto/IPassagewayDTO';
import {Passageway} from "../domain/passageway";
import IPassagewayRepo from "../services/IRepos/IPassagewayRepo";
import IPassagewayService from "./IServices/IPassagewayService";
import {Result} from "../core/logic/Result";
import {PassagewayMapper} from "../mappers/PassagewayMapper";
import IFloorRepo from "./IRepos/IFloorRepo";


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

      const floor1 = await this.FloorRepo.findByDomainId(PassagewayResult.floorID1);

      if (floor1 == null) {
        return Result.fail<IPassagewayDTO>('Floor1 not found');
      }
      const floor2 = await this.FloorRepo.findByDomainId(PassagewayResult.floorID2);

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
}
