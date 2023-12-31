using DDDSample1.Domain.Shared;

using System;

namespace DDDSample1.Domain.SystemUser
{
    public class UserId : EntityId
    {
        public UserId(string value) : base(value)
        {
        }

        protected override object CreateFromString(string text)
        {
            return text; // Assuming UserId is a string-based identifier
        }

        public override string AsString()
        {
            return (string)ObjValue; // Assuming UserId is a string-based identifier
        }

        // Additional logic for generating a UserId from a Guid
        public static UserId NewUserId()
        {
            return new UserId(Guid.NewGuid().ToString());
        }
    }
}
