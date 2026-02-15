{-# LANGUAGE TemplateHaskell #-}
import Generics.BiGUL
import Generics.BiGUL.Interpreter

main :: IO ()
main = do
    -- 简单的 put 测试
    let s = 1     -- 源
    let v = 99    -- 视图
    let resPut = put Replace s v
    print resPut  -- 期望输出：Right 99

    -- 简单的 get 测试
    let resGet = get Replace s
    print resGet  -- 期望输出：Right 1
