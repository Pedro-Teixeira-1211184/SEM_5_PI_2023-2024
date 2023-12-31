using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.SystemUser {

    public interface IUserRepository: IRepository<User, UserId> {
        
    }
}