import {Mapper} from "../core/infra/Mapper";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {SignUpRequest} from "../domain/signUpRequest";
import {ISignUpRequestDTO} from "../dto/ISignUpRequestDTO";

export class SignUpRequestMap extends Mapper<SignUpRequest> {

  public static toDTO(request: SignUpRequest): ISignUpRequestDTO {
    return {
      //id: user.id.toString(),
      firstName: request.firstName,
      lastName: request.lastName,
      email: request.email,
      password: request.password,
    } as ISignUpRequestDTO;
  }

  public static async toDomain(raw: any): Promise<SignUpRequest> {
    const userOrError = SignUpRequest.create(
      {
        domainId: raw.signUpRequestDomainId,
        firstName: raw.signUpRequestFirstName,
        lastName: raw.signUpRequestLastName,
        email: raw.signUpRequestEmail,
        password: raw.signUpRequestPassword
      },
      new UniqueEntityID(raw.domainId)
    );

    userOrError.isFailure ? console.log(userOrError.error) : '';

    return userOrError.isSuccess ? userOrError.getValue() : null;
  }

  public static toPersistence(user: SignUpRequest): any {
    return {
      signUpRequestDomainId: user.id.toString(),
      signUpRequestEmail: user.email,
      signUpRequestPassword: user.password,
      signUpRequestFirstName: user.firstName,
      signUpRequestLastName: user.lastName
    };
  }
}
